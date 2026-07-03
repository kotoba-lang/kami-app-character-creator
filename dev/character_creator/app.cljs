(ns character-creator.app
  "The real, interactive in-browser character-creation screen — ADR-2607031200,
   /loop maturity pass. Everything built across this evening's sessions
   (`character-creator.doc/pipeline/gpu-adapter/expression-bridge/thumbnail/
   persistence`, `kami-ui-sdk.widgets`'s real DOM slider/color-swatch/carousel,
   `kami.webgpu.mesh`'s skin+morph WebGPU executor) was pipeline code, unit
   tests, or a hardcoded auto-cycling demo (`preview_demo.cljs`) — no screen a
   person could sit down and use to make a character. This is that screen:
   move a slider or click a swatch, the live-rendered 3D avatar updates.

   Dev-only entry (`:cljs` alias, same build infra as `preview_demo.cljs`,
   which is kept unchanged as a separate auto-cycling regression reference —
   not superseded, since it exercises the auto-crossfade/pose-animation paths
   this interactive screen deliberately keeps static (see `render-loop!`).

   Field/preset choice: `character.params/default-character-def`'s fields
   were verified against real call sites before exposing them as sliders —
   `character/generate-character` passes `:face`/`:eyes`/`:nose`/`:mouth` into
   `character.blendshape/apply-*-shape` and `:body` into `character.body/
   generate-body`, so every slider below genuinely changes generated geometry
   (not a dead param — checked by reading blendshape.cljc/body.cljc/
   character.cljc directly, not assumed)."
  (:require [character-creator.doc :as doc]
            [character-creator.pipeline :as pipeline]
            [character-creator.gpu-adapter :as gpu]
            [character-creator.expression-bridge :as bridge]
            [character-creator.thumbnail :as thumb]
            [character-creator.persistence :as persist]
            [character-creator.accessories :as acc]
            [character.params :as params]
            [character.body :as cbody]
            [kami.webgpu.mesh :as mesh]
            [kami-ui-sdk.widgets :as w]
            [kami-ui-sdk.ui :as ui]))

;; ─── state ──────────────────────────────────────────────────────────────

(defonce !doc (atom (doc/default-character-doc)))
(defonce !active-preset (atom :neutral))
(defonce !buffers (atom nil))       ;; {:head <mesh.upload-mesh! result> :body ...}
(defonce !morph-names (atom nil))   ;; index-aligned with head morph-target-deltas
(defonce !fit (atom nil))           ;; {:head-fit {:center :scale} :body-fit ...} — computed
                                     ;; ONCE from the default doc and reused on every regen, so a
                                     ;; slider change visibly deforms the mesh rather than the
                                     ;; camera silently re-centering/re-zooming to compensate.
(defonce !n-bones (atom 13))
(defonce !bone-world-pos (atom nil))  ;; name -> [x y z], rest pose — set once at
                                     ;; init (0-arity skeleton, matching !n-bones'
                                     ;; own simplification); used to resolve a
                                     ;; decal's pattern gradient centre at draw
                                     ;; time (character-creator.accessories/
                                     ;; draw-pattern), not just its placement.
(defonce !widgets (atom {}))        ;; key -> widget instance ({:set-value ...}), so Randomize can
                                     ;; push new values into the DOM, not just mutate !doc invisibly

(defn- log [& xs] (.apply (.-log js/console) js/console (clj->js xs)))

;; ─── small pure helpers (duplicated from `preview_demo.cljs`, which keeps
;; them `defn-` private — small enough (bbox fit + hex<->float color) that
;; re-deriving here beats a cross-dev-file coupling) ------------------------

(defn- bbox-fit [positions]
  (let [xs (map first positions) ys (map second positions) zs (map #(nth % 2) positions)
        lo [(apply min xs) (apply min ys) (apply min zs)]
        hi [(apply max xs) (apply max ys) (apply max zs)]
        center (mapv (fn [a b] (/ (+ a b) 2.0)) lo hi)
        extent (apply max (map - hi lo))]
    {:center center :scale (/ 1.2 (max extent 1e-6))}))

(defn- fit-positions [positions {:keys [center scale]}]
  (mapv (fn [p] (mapv (fn [pi ci] (* (- pi ci) scale)) p center)) positions))

(defn- fit-deltas [deltas scale]
  (mapv (fn [target] (mapv (fn [d] (mapv #(* % scale) d)) target)) deltas))

(defn- clamp01 [x] (max 0.0 (min 1.0 x)))
(defn- ch->hex [c]
  (let [s (.toString (js/Math.round (* (clamp01 c) 255)) 16)]
    (if (= 1 (count s)) (str "0" s) s)))
(defn- rgb->hex [[r g b]] (str "#" (ch->hex r) (ch->hex g) (ch->hex b)))
(defn- hex->rgb [hex]
  (let [n (js/parseInt (subs hex 1) 16)]
    [(/ (bit-and (bit-shift-right n 16) 0xFF) 255.0)
     (/ (bit-and (bit-shift-right n 8) 0xFF) 255.0)
     (/ (bit-and n 0xFF) 255.0)]))

(defn- identity-mat4 [] (js/Float32Array. #js [1 0 0 0, 0 1 0 0, 0 0 1 0, 0 0 0 1]))

;; ─── accessories/decals: head-group vs body-group routing -----------------
;; The render loop (below) draws head and body as two separately-fitted/
;; -translated "portraits" side by side, not one composited standing figure
;; (an existing choice this fn doesn't change) — hair already follows this by
;; sharing `head-fit`/`head-mvp`. An accessory/decal must do the same: which
;; fit/translate it uses depends on which bone it's attached to.

(def ^:private head-attach-bones #{"head" "leftEye" "rightEye" "jaw"})

(defn- extra-group
  "`:head` or `:body`, from `character-creator.accessories`' catalog
   `:attach-bone` for `id` (an equip or decal id)."
  [id]
  (let [attach (:attach-bone (or (get acc/accessory-catalog id) (get acc/decal-catalog id)))]
    (if (contains? head-attach-bones attach) :head :body)))

(defn- extra-color
  "Flat draw-call colour for `id` — this shader takes one colour per draw
   call (see the palette-colour comment in the render loop below), not
   per-vertex material data, so an accessory/decal draws in its own catalog
   `:base-color` (RGB, alpha dropped)."
  [id]
  (let [[r g b] (:base-color (or (get acc/accessory-catalog id) (get acc/decal-catalog id)))]
    [r g b]))

;; ─── regen: CharacterDoc -> re-uploaded GPU buffers -----------------------
;; Public entry point every control's `:on-change` calls. No debouncing —
;; per directive, measure real timing first (logged below) and only add
;; rAF-coalescing if the browser test actually shows drag-jank; see the
;; console `regen took ... ms` line this fn logs on every call.

(defn regen! [mctx]
  (let [t0 (js/performance.now)
        vdoc (pipeline/character-doc->vrm-document @!doc)
        head-geo (gpu/head-mesh-geometry vdoc)
        body-geo (gpu/body-mesh-geometry vdoc)
        hair-geo (gpu/hair-mesh-geometry vdoc) ;; nil for :bald — see its docstring
        head-fit (:head-fit @!fit)
        body-fit (:body-fit @!fit)
        head-buffers (mesh/upload-mesh!
                       mctx {:positions (fit-positions (:positions head-geo) head-fit)
                             :normals (:normals head-geo)
                             :indices (:indices head-geo)
                             :morph-target-deltas (fit-deltas (:morph-target-deltas head-geo) (:scale head-fit))})
        body-buffers (mesh/upload-mesh!
                       mctx {:positions (fit-positions (:positions body-geo) body-fit)
                             :normals (:normals body-geo)
                             :indices (:indices body-geo)
                             :joints (:joints body-geo)
                             :weights (:weights body-geo)})
        ;; hair shares the head's fit (same local coordinate space — it must stay
        ;; spatially aligned with the head it's attached to, not get independently
        ;; re-centered/re-scaled by its own bounding box).
        hair-buffers (when hair-geo
                       (mesh/upload-mesh!
                        mctx {:positions (fit-positions (:positions hair-geo) head-fit)
                              :normals (:normals hair-geo)
                              :indices (:indices hair-geo)}))
        ;; accessories/decals: same mesh-geometry-by-name reader `hair` uses
        ;; (works unchanged since accessory/decal MeshParts are shaped like
        ;; any other part), each fit'd via whichever of head-fit/body-fit its
        ;; `extra-group` says it belongs to.
        extra-ids (into (vec (:character/equip @!doc)) (:character/decals @!doc))
        extras (into {}
                     (keep (fn [id]
                             (when-let [geo (gpu/mesh-geometry-by-name vdoc (name id))]
                               (let [fit (if (= :head (extra-group id)) head-fit body-fit)]
                                 [id {:buffers (mesh/upload-mesh!
                                                mctx {:positions (fit-positions (:positions geo) fit)
                                                      :normals (:normals geo)
                                                      :indices (:indices geo)})
                                      :group (extra-group id)
                                      :color (extra-color id)}])))
                           extra-ids))
        dt (- (js/performance.now) t0)]
    (reset! !morph-names (:morph-target-names head-geo))
    (reset! !buffers {:head head-buffers :body body-buffers :hair hair-buffers :extras extras})
    (log "regen took" (.toFixed dt 1) "ms — head verts:" (count (:positions head-geo))
         "body verts:" (count (:positions body-geo))
         "hair verts:" (if hair-geo (count (:positions hair-geo)) 0)
         "extras:" (vec (keys extras)))
    (set! (.-__kami_cc_last_regen_ms js/window) dt)
    (set! (.-__kami_cc_regen_count js/window) (inc (or (.-__kami_cc_regen_count js/window) 0)))
    (set! (.-__kami_cc_last_hair_verts js/window) (if hair-geo (count (:positions hair-geo)) 0))
    (set! (.-__kami_cc_last_hair_preset js/window) (name (get-in @!doc [:character/def :hair :preset])))
    (set! (.-__kami_cc_last_extras js/window) (clj->js (mapv name (keys extras))))))

(defn- update-def! [mctx path v]
  (swap! !doc assoc-in (into [:character/def] path) v)
  (regen! mctx))

(def ^:private palette-key->def-path
  "Mirrors `character-creator.doc/apply-palette`'s mapping (private there —
   `boot-config`-only — so re-stated here for a live doc's in-place update)."
  {:skin [:skin :tone] :hair [:hair :color] :eye [:eyes :iris-color]})

(defn- update-palette! [mctx k hex]
  (let [rgb (hex->rgb hex)]
    (swap! !doc (fn [d]
                  (-> d
                      (assoc-in [:character/palette k] rgb)
                      (assoc-in (into [:character/def] (palette-key->def-path k)) rgb)))))
  (regen! mctx))

;; ─── control panel ─────────────────────────────────────────────────────

(def ^:private sliders
  "`[path label min max step]` — the subset of `CharacterDef` fields verified
   (see namespace docstring) to genuinely affect `generate-character`'s
   output. Not exhaustive (e.g. `:face :cheekbone-width`/`:temple-width` and
   several others are equally real but omitted to keep the panel usable)."
  [[[:body :height] "Height" 0.6 1.4 0.01]
   [[:body :shoulder-width] "Shoulder width" 0.0 1.0 0.01]
   [[:body :build] "Build" 0.0 1.0 0.01]
   [[:body :neck-thickness] "Neck thickness" 0.0 1.0 0.01]
   [[:face :jaw-width] "Jaw width" 0.0 1.0 0.01]
   [[:eyes :depth] "Eye depth" 0.0 1.0 0.01]
   [[:nose :bridge-height] "Nose bridge" 0.0 1.0 0.01]
   [[:mouth :width] "Mouth width" 0.0 1.0 0.01]])

(defn- get-def [path] (get-in @!doc (into [:character/def] path)))

(defn- panel-heading [container text]
  (let [h (.createElement js/document "div")]
    (set! (.-textContent h) text)
    (.setProperty (.-style h) "font-weight" "900")
    (.setProperty (.-style h) "font-size" "13px")
    (.setProperty (.-style h) "color" (:text-primary ui/theme))
    (.setProperty (.-style h) "margin-top" "10px")
    (.appendChild container h)
    h))

(defn- hair-thumbnail-fn
  "`(fn [stage-el] node)` — real use of `Carousel`'s procedural-thumbnail
   signature: renders `character-creator.thumbnail/character-doc->sprite`'s
   flat-primitive sprite (for the *current* doc with this preset substituted)
   as an inline SVG, not a baked image."
  [preset]
  (fn [_stage-el]
    (let [doc-with-preset (assoc-in @!doc [:character/def :hair :preset] preset)
          sprite (thumb/character-doc->sprite doc-with-preset)
          svg (.createElementNS js/document "http://www.w3.org/2000/svg" "svg")]
      (.setAttribute svg "viewBox" "0 0 100 100")
      (.setAttribute svg "width" "72") (.setAttribute svg "height" "72")
      (doseq [{:keys [shape cx cy w h r color]} sprite]
        (let [[cr cg cb] color
              fill (rgb->hex [cr cg cb])
              el (.createElementNS js/document "http://www.w3.org/2000/svg"
                                    (if (= shape :circle) "circle" "rect"))]
          (if (= shape :circle)
            (do (.setAttribute el "cx" cx) (.setAttribute el "cy" cy) (.setAttribute el "r" r))
            (do (.setAttribute el "x" (- cx (/ w 2))) (.setAttribute el "y" (- cy (/ h 2)))
                (.setAttribute el "width" w) (.setAttribute el "height" h) (.setAttribute el "rx" 4)))
          (.setAttribute el "fill" fill)
          (.appendChild svg el)))
      svg)))

;; ─── equip/decal slot picking — one `carousel!` per SLOT (not per catalog
;; entry), each including a "None" option, so choosing e.g. a cap doesn't
;; block also wearing a necklace (different slots) but does replace any
;; other headwear (same slot) — a small, deliberately-scoped equip-slot
;; system, not arbitrary multi-select checkboxes (`carousel!` is single-
;; value; this is the natural fit for it). ------------------------------

(def ^:private equip-slots
  "slot-key -> the `character-creator.accessories/accessory-catalog` ids
   that occupy it (mutually exclusive within a slot, independent across
   slots)."
  {:headwear [:cap-simple :glasses-round]
   :jewelry [:earring-stud :necklace-simple]})

(defn- set-equip-slot! [mctx slot id]
  (swap! !doc update :character/equip
         (fn [equip]
           (let [without-slot (vec (remove (set (get equip-slots slot)) equip))]
             (if id (conj without-slot id) without-slot))))
  (regen! mctx))

(defn- set-decal! [mctx id]
  ;; single decal at a time — a real multi-decal picker needs a different
  ;; (checkbox-style) widget than the single-value `carousel!`; scoped down
  ;; deliberately, `character-creator.pipeline` itself supports a full
  ;; vector, this UI just doesn't expose more than one slot yet.
  (swap! !doc assoc :character/decals (if id [id] []))
  (regen! mctx))

(defn- slot-carousel!
  [container mctx {:keys [test-id ids current on-select]}]
  (let [row (.createElement js/document "div")]
    (.setAttribute row "data-testid" test-id)
    (.appendChild container row)
    (w/carousel! row
      {:items (into [{:id :none :label "None"}]
                    (mapv (fn [id] {:id id :label (name id)}) ids))
       :value (or current :none)
       :on-change (fn [item] (on-select (when-not (= :none (:id item)) (:id item))))})))

;; ─── save/load/randomize — makes the already-built pieces usable end-to-end
;; (ADR-2607031200 Phase 1 shipped persist/save-local!, load-local,
;; download-edn!/-vrm! but nothing called save-local!/load-local from the UI
;; — only the .vrm export button was wired). ---------------------------------

(defn- sync-widget-values!
  "Push @!doc's current values into every captured widget's own displayed
   state (kami-ui-sdk.widgets' :set-value, not raw DOM/atom mutation) — used
   by both Load and Randomize so a slider visibly jumps to its new position
   instead of the doc changing invisibly underneath a stale-looking control."
  []
  (doseq [[path {:keys [set-value]}] (:sliders @!widgets)]
    (set-value (get-in @!doc (into [:character/def] path))))
  (doseq [[k {:keys [set-value]}] (:swatches @!widgets)]
    (set-value (rgb->hex (get (:character/palette @!doc) k))))
  (when-let [{:keys [set-value]} (:hair-carousel @!widgets)]
    (set-value (get-in @!doc [:character/def :hair :preset])))
  (when-let [{:keys [set-value]} (:headwear-carousel @!widgets)]
    (set-value (or (some (set (:headwear equip-slots)) (:character/equip @!doc)) :none)))
  (when-let [{:keys [set-value]} (:jewelry-carousel @!widgets)]
    (set-value (or (some (set (:jewelry equip-slots)) (:character/equip @!doc)) :none)))
  (when-let [{:keys [set-value]} (:decal-carousel @!widgets)]
    (set-value (or (first (:character/decals @!doc)) :none)))
  (when-let [{:keys [set-value]} (:expr-carousel @!widgets)]
    (set-value @!active-preset)))

(defn- save! []
  (persist/save-local! @!doc)
  (set! (.-__kami_cc_last_save js/window) (:character/id @!doc)))

(defn- load! [mctx]
  ;; No multi-character picker exists in this screen yet (single doc/id at a
  ;; time) — "Load" reloads whatever was last saved under the CURRENT
  ;; character's own :character/id, i.e. "load last saved," per directive.
  ;; A real character-switcher (persist/list-local-ids already supports
  ;; multiple ids) is a follow-up, not fabricated here.
  (when-let [loaded (persist/load-local (:character/id @!doc))]
    (reset! !doc loaded)
    (sync-widget-values!)
    (regen! mctx)
    (set! (.-__kami_cc_last_load js/window) (:character/id loaded))))

(defn- rand-channel [] (+ 0.15 (* (js/Math.random) 0.75)))
(defn- rand-hex [] (rgb->hex [(rand-channel) (rand-channel) (rand-channel)]))

(defn- randomize! [mctx]
  (let [hair-list (vec params/hair-presets)
        decal-list (into [nil] (keys acc/decal-catalog))
        expr-list (vec (keys bridge/preset->arkit-weights))
        new-def (reduce (fn [d [path _ lo hi step]]
                           (let [raw (+ lo (* (js/Math.random) (- hi lo)))
                                 snapped (* step (js/Math.round (/ raw step)))]
                             (assoc-in d path (-> snapped (max lo) (min hi)))))
                         (:character/def @!doc)
                         sliders)
        skin-hex (rand-hex) hair-hex (rand-hex) eye-hex (rand-hex)
        new-def (-> new-def
                    (assoc-in [:skin :tone] (hex->rgb skin-hex))
                    (assoc-in [:hair :color] (hex->rgb hair-hex))
                    (assoc-in [:eyes :iris-color] (hex->rgb eye-hex))
                    (assoc-in [:hair :preset] (rand-nth hair-list)))
        headwear (rand-nth [nil :cap-simple :glasses-round])
        jewelry (rand-nth [nil :earring-stud :necklace-simple])
        decal (rand-nth decal-list)
        expr (rand-nth expr-list)]
    (swap! !doc
           (fn [d]
             (-> d
                 (assoc :character/def new-def)
                 (assoc-in [:character/palette :skin] (hex->rgb skin-hex))
                 (assoc-in [:character/palette :hair] (hex->rgb hair-hex))
                 (assoc-in [:character/palette :eye] (hex->rgb eye-hex))
                 (assoc :character/equip (vec (remove nil? [headwear jewelry])))
                 (assoc :character/decals (if decal [decal] [])))))
    (reset! !active-preset expr)
    (sync-widget-values!)
    (regen! mctx)
    (set! (.-__kami_cc_randomize_count js/window) (inc (or (.-__kami_cc_randomize_count js/window) 0)))))

(defn- mk-button! [container {:keys [test-id label bg on-click]}]
  (let [btn (.createElement js/document "button")]
    (.setAttribute btn "data-testid" test-id)
    (set! (.-textContent btn) label)
    (.setProperty (.-style btn) "background" bg)
    (.setProperty (.-style btn) "border" "none")
    (.setProperty (.-style btn) "border-radius" (:radius-small ui/theme))
    (.setProperty (.-style btn) "padding" "10px 14px")
    (.setProperty (.-style btn) "font-weight" "800")
    (.setProperty (.-style btn) "cursor" "pointer")
    (.setProperty (.-style btn) "color" "#fff")
    (.setProperty (.-style btn) "margin-right" "8px")
    (.addEventListener btn "click" on-click)
    (.appendChild container btn)
    btn))

(defn- build-controls! [container mctx]
  (panel-heading container "Body / Face")
  (doseq [[path label lo hi step] sliders]
    (let [row (.createElement js/document "div")]
      (.setAttribute row "data-testid" (str "slider-" (name (last path))))
      (.appendChild container row)
      (swap! !widgets assoc-in [:sliders path]
             (w/slider! row {:label label :min lo :max hi :step step :value (get-def path)
                              :on-change (fn [v] (update-def! mctx path v))}))))

  (panel-heading container "Colors")
  (let [row (.createElement js/document "div")]
    (.appendChild container row)
    (doseq [[k lbl] [[:skin "Skin"] [:hair "Hair"] [:eye "Eye"]]]
      (let [wrap (.createElement js/document "div")
            lab (.createElement js/document "span")]
        (.setAttribute wrap "data-testid" (str "swatch-row-" (name k)))
        (.setProperty (.-style wrap) "display" "flex")
        (.setProperty (.-style wrap) "align-items" "center")
        (.setProperty (.-style wrap) "gap" "6px")
        (set! (.-textContent lab) lbl)
        (.setProperty (.-style lab) "font-size" "11px")
        (.setProperty (.-style lab) "width" "34px")
        (.appendChild wrap lab)
        (.appendChild row wrap)
        (swap! !widgets assoc-in [:swatches k]
               (w/color-swatch! wrap
                 {:presets [(rgb->hex (get (:character/palette @!doc) k))
                            "#f4c9a8" "#ecd0b0" "#caa27a" "#8a5a3a" "#3f2a20"]
                  :value (rgb->hex (get (:character/palette @!doc) k))
                  :on-change (fn [hex] (update-palette! mctx k hex))})))))

  (panel-heading container "Hair style")
  (let [row (.createElement js/document "div")]
    (.setAttribute row "data-testid" "hair-carousel")
    (.appendChild container row)
    (swap! !widgets assoc :hair-carousel
           (w/carousel! row
             {:items (mapv (fn [p] {:id p :label (name p) :thumbnail (hair-thumbnail-fn p)})
                            (sort params/hair-presets))
              :value (get-def [:hair :preset])
              :on-change (fn [item] (update-def! mctx [:hair :preset] (:id item)))})))

  (panel-heading container "Headwear")
  (swap! !widgets assoc :headwear-carousel
         (slot-carousel! container mctx
           {:test-id "headwear-carousel" :ids (:headwear equip-slots)
            :current (some (set (:headwear equip-slots)) (:character/equip @!doc))
            :on-select (fn [id] (set-equip-slot! mctx :headwear id))}))

  (panel-heading container "Jewelry")
  (swap! !widgets assoc :jewelry-carousel
         (slot-carousel! container mctx
           {:test-id "jewelry-carousel" :ids (:jewelry equip-slots)
            :current (some (set (:jewelry equip-slots)) (:character/equip @!doc))
            :on-select (fn [id] (set-equip-slot! mctx :jewelry id))}))

  (panel-heading container "Tattoo / Scar")
  (swap! !widgets assoc :decal-carousel
         (slot-carousel! container mctx
           {:test-id "decal-carousel" :ids (keys acc/decal-catalog)
            :current (first (:character/decals @!doc))
            :on-select (fn [id] (set-decal! mctx id))}))

  (panel-heading container "Expression preview")
  (let [row (.createElement js/document "div")]
    (.setAttribute row "data-testid" "expr-carousel")
    (.appendChild container row)
    (swap! !widgets assoc :expr-carousel
           (w/carousel! row
             {:items (mapv (fn [p] {:id p :label (name p)}) (sort (keys bridge/preset->arkit-weights)))
              :value :neutral
              :on-change (fn [item] (reset! !active-preset (:id item)))})))

  (panel-heading container "Randomize")
  (mk-button! container
    {:test-id "randomize-btn" :label "🎲 Randomize" :bg (get-in ui/theme [:accent :purple])
     :on-click (fn [_] (randomize! mctx))})

  (panel-heading container "Save / Load")
  (let [row (.createElement js/document "div")]
    (.appendChild container row)
    (mk-button! row {:test-id "save-btn" :label "Save" :bg (get-in ui/theme [:accent :blue])
                      :on-click (fn [_] (save! ))})
    (mk-button! row {:test-id "load-btn" :label "Load" :bg (get-in ui/theme [:accent :blue])
                      :on-click (fn [_] (load! mctx))}))

  (panel-heading container "Export")
  (let [row (.createElement js/document "div")]
    (.appendChild container row)
    (mk-button! row
      {:test-id "export-btn" :label "Download .vrm" :bg (get-in ui/theme [:accent :green])
       :on-click (fn [_] (persist/download-vrm! @!doc (pipeline/character-doc->vrm-bytes @!doc))
                         (set! (.-__kami_cc_exported js/window) true))})
    (mk-button! row
      {:test-id "export-edn-btn" :label "Download .edn" :bg (get-in ui/theme [:accent :green])
       :on-click (fn [_] (persist/download-edn! @!doc)
                         (set! (.-__kami_cc_exported_edn js/window) true))})))

;; ─── main: WebGPU bootstrap, control panel, render loop (static pose — this
;; screen edits appearance, it isn't the auto-animating demo `preview_demo.
;; cljs` already covers) ----------------------------------------------------

(defn ^:export main []
  (let [canvas (js/document.getElementById "gpu-canvas")
        controls (js/document.getElementById "controls")]
    (set! (.-fontFamily (.-style (.-body js/document))) (:font ui/theme))
    (set! (.-background (.-style (.-body js/document))) (:bg ui/theme))
    (if-not (.-gpu js/navigator)
      (do (log "WebGPU not available")
          (set! (.-innerText (js/document.getElementById "status")) "WebGPU NOT available in this browser."))
      (-> (.requestAdapter (.-gpu js/navigator))
          (.then (fn [adapter] (.requestDevice adapter)))
          (.then
            (fn [device]
              (let [gpu (.-gpu js/navigator)
                    fmt (.getPreferredCanvasFormat gpu)
                    w (.-clientWidth canvas) h (.-clientHeight canvas)
                    _ (set! (.-width canvas) w)
                    _ (set! (.-height canvas) h)
                    ctx (.getContext canvas "webgpu")
                    _ (.configure ctx #js {:device device :format fmt :alphaMode "opaque"})
                    depth-tex (.createTexture device #js {:size #js [w h] :format "depth24plus"
                                                           :usage (.-RENDER_ATTACHMENT js/GPUTextureUsage)})
                    mctx (mesh/init! device fmt)
                    skeleton (cbody/generate-humanoid-skeleton)
                    _ (reset! !n-bones (count (:bones skeleton)))
                    _ (reset! !bone-world-pos
                              (zipmap (mapv :name (:bones skeleton))
                                      (cbody/bone-world-positions (:bones skeleton))))
                    vdoc0 (pipeline/character-doc->vrm-document @!doc)
                    head-geo0 (gpu/head-mesh-geometry vdoc0)
                    body-geo0 (gpu/body-mesh-geometry vdoc0)
                    _ (reset! !fit {:head-fit (bbox-fit (:positions head-geo0))
                                     :body-fit (bbox-fit (:positions body-geo0))})]
                (build-controls! controls mctx)
                (regen! mctx)
                (set! (.-innerText (js/document.getElementById "status")) "WebGPU OK — interactive.")
                (set! (.-__kami_cc_ready js/window) true)
                ((fn tick []
                   (js/requestAnimationFrame
                     (fn [_]
                       (when-let [{:keys [head body hair extras]} @!buffers]
                         (let [hair-mesh hair ;; disambiguate from the palette `:hair` color, below
                               vp (mesh/view-projection [0 0.25 2.4] [0 0 0] (/ w (max 1 h)))
                               translate (fn [tx ty] (js/Float32Array. #js [1 0 0 0, 0 1 0 0, 0 0 1 0, tx ty 0 1]))
                               m4-mul (fn [a b]
                                        (let [o (js/Float32Array. 16)]
                                          (dotimes [c 4]
                                            (dotimes [r 4]
                                              (aset o (+ (* c 4) r)
                                                    (+ (* (aget a r) (aget b (+ (* c 4) 0)))
                                                       (* (aget a (+ r 4)) (aget b (+ (* c 4) 1)))
                                                       (* (aget a (+ r 8)) (aget b (+ (* c 4) 2)))
                                                       (* (aget a (+ r 12)) (aget b (+ (* c 4) 3)))))))
                                          o))
                               head-mvp (m4-mul vp (translate -0.55 0.15))
                               body-mvp (m4-mul vp (translate 0.55 -0.15))
                               ;; hair shares the head's translate (it sits on the head, same as
                               ;; `regen!` sharing `head-fit` between the two meshes)
                               hair-mvp head-mvp
                               weights (gpu/preset-weight-vector bridge/preset->arkit-weights @!morph-names @!active-preset)
                               joints (vec (repeat @!n-bones (identity-mat4)))
                               ;; real palette colors (were hardcoded literals — a swatch click
                               ;; previously had zero visible effect because of this), read fresh
                               ;; every frame so a color-swatch change shows up immediately without
                               ;; needing a mesh regen (this shader takes one flat draw-call color,
                               ;; not per-vertex material data).
                               {:keys [skin hair]} (:character/palette @!doc)
                               hair-color hair
                               enc (.createCommandEncoder device)
                               view (.createView (.getCurrentTexture ctx))
                               pass (.beginRenderPass enc
                                      #js {:colorAttachments #js [#js {:view view :loadOp "clear" :storeOp "store"
                                                                       :clearValue #js {:r 0.94 :g 0.92 :b 0.86 :a 1}}]
                                           :depthStencilAttachment #js {:view (.createView depth-tex) :depthLoadOp "clear"
                                                                        :depthStoreOp "store" :depthClearValue 1.0}})]
                           (mesh/draw! mctx pass head head-mvp skin weights [])
                           (mesh/draw! mctx pass body body-mvp skin [] joints)
                           (when hair-mesh (mesh/draw! mctx pass hair-mesh hair-mvp hair-color [] []))
                           (doseq [[id {:keys [buffers group color]}] extras]
                             ;; decal-catalog entries may carry a :pattern (radial
                             ;; gradient fade, e.g. :scar-cheek/:tattoo-arm-band) —
                             ;; acc/draw-pattern resolves it against the SAME rest-
                             ;; pose bone positions the mesh itself was baked
                             ;; relative to, or returns nil for plain-color parts
                             ;; (every accessory, and decals with no :pattern
                             ;; entry) so they draw exactly as before.
                             (let [pattern (acc/draw-pattern id @!bone-world-pos)]
                               (mesh/draw! mctx pass buffers (if (= :head group) head-mvp body-mvp) color [] [] pattern)))
                           (.end pass)
                           (.submit (.-queue device) #js [(.finish enc)])))
                       (tick))))
                 ))))
          (.catch (fn [err] (log "WebGPU init failed:" err)
                    (set! (.-innerText (js/document.getElementById "status")) (str "WebGPU init failed: " err))))))))

(main)
