(ns character-creator.accessories
  "Procedural, asset-free accessory (`:character/equip`) and decal
   (`:character/decals`) meshes — closes the ADR-2607031200 original sketch's
   deferred `:character/equip` (\"スロット参照。Phase 1 は装備メッシュ差し替え
   無しの固定セットで可\" — Phase 1 explicitly punted on any real equip-mesh
   system). Same philosophy as `character-creator.thumbnail`/`kami-isekai-
   assets`'s chargen: simple composed primitives, no baked assets.

   Geometry lives here (not in `kotoba-lang/character`) because none of this
   is a port of anything — no Rust original ever had an accessory system, and
   these are app-level content, not restored domain logic.

   **Decals are a deliberate, honestly-scoped approximation, not real
   texture/UV decals.** The current material system is flat-per-part-color
   with no texture/UV-mapped decal pipeline (building one is a much larger,
   separate effort). A 'tattoo' or 'scar' here is a small flat quad offset a
   hair's-width outward from the body surface at a fixed anchor + a manually
   chosen outward-normal direction, with its own distinct flat material
   colour. It reads as a small coloured patch near the right anatomical area,
   not as ink/skin texture detail — say so, don't oversell it."
  (:require [character.body :as cbody]))

;; ── tiny local mesh-generation helpers (standalone — do not touch
;; `character.body`, another /loop fork owns that file this iteration; these
;; are simple enough not to be worth a cross-repo dependency either) ────────

(defn- v3+ [[ax ay az] [bx by bz]] [(+ ax bx) (+ ay by) (+ az bz)])
(defn- v3* [[x y z] s] [(* x s) (* y s) (* z s)])
(defn- v3-norm [[x y z]]
  (let [l (max 1e-9 (Math/sqrt (+ (* x x) (* y y) (* z z))))]
    [(/ x l) (/ y l) (/ z l)]))
(defn- v3-cross [[ax ay az] [bx by bz]]
  [(- (* ay bz) (* az by)) (- (* az bx) (* ax bz)) (- (* ax by) (* ay bx))])

(defn- sphere-mesh
  "UV sphere, radius `r`, `n-lat` latitude bands x `n-lon` longitude
   segments (explicit pole vertices, standard quad-strip-between-rings
   indexing). Local space, centred at origin."
  [r n-lat n-lon]
  (let [top {:position [0.0 r 0.0] :normal [0.0 1.0 0.0] :uv [0.5 0.0]}
        bottom {:position [0.0 (- r) 0.0] :normal [0.0 -1.0 0.0] :uv [0.5 1.0]}
        rings (for [i (range 1 n-lat)
                    :let [lat (- (/ (* Math/PI i) n-lat) (/ Math/PI 2.0))
                          y (* r (Math/sin lat)) ring-r (* r (Math/cos lat))]]
                (mapv (fn [j]
                        (let [lon (* 2.0 Math/PI (/ j n-lon))
                              x (* ring-r (Math/cos lon)) z (* ring-r (Math/sin lon))]
                          {:position [x y z] :normal (v3-norm [x y z])
                           :uv [(/ j n-lon) (/ i n-lat)]}))
                      (range n-lon)))
        ring-vecs (vec rings)
        n-rings (count ring-vecs)
        vertices (into [top] (into (vec (apply concat ring-vecs)) [bottom]))
        top-idx 0
        ring-start (fn [ri] (+ 1 (* ri n-lon)))
        bottom-idx (dec (count vertices))
        top-fan (mapcat (fn [j] [top-idx (+ (ring-start 0) j) (+ (ring-start 0) (mod (inc j) n-lon))])
                         (range n-lon))
        mid (mapcat (fn [ri]
                      (mapcat (fn [j]
                                (let [a (+ (ring-start ri) j) b (+ (ring-start ri) (mod (inc j) n-lon))
                                      c (+ (ring-start (inc ri)) j) d (+ (ring-start (inc ri)) (mod (inc j) n-lon))]
                                  [a c b b c d]))
                              (range n-lon)))
                    (range (dec n-rings)))
        bottom-fan (mapcat (fn [j] [bottom-idx (+ (ring-start (dec n-rings)) (mod (inc j) n-lon))
                                     (+ (ring-start (dec n-rings)) j)])
                            (range n-lon))]
    {:vertices vertices :indices (vec (concat top-fan mid bottom-fan))}))

(defn- torus-mesh
  "Torus, major radius `R` (ring radius), minor radius `r` (tube radius),
   `n-major` x `n-minor` grid. Local space, ring lies in the XY plane
   (axis = local Z), centred at origin."
  [R r n-major n-minor]
  (let [verts (vec (for [i (range n-major) j (range n-minor)]
                      (let [theta (* 2.0 Math/PI (/ i n-major))
                            phi (* 2.0 Math/PI (/ j n-minor))
                            ct (Math/cos theta) st (Math/sin theta)
                            cp (Math/cos phi) sp (Math/sin phi)
                            x (* (+ R (* r cp)) ct) y (* (+ R (* r cp)) st) z (* r sp)]
                        {:position [x y z] :normal [(* cp ct) (* cp st) sp]
                         :uv [(/ i n-major) (/ j n-minor)]})))
        idx (fn [i j] (+ (* (mod i n-major) n-minor) (mod j n-minor)))
        indices (vec (mapcat (fn [i]
                                (mapcat (fn [j]
                                          (let [a (idx i j) b (idx (inc i) j)
                                                c (idx i (inc j)) d (idx (inc i) (inc j))]
                                            [a b c b d c]))
                                        (range n-minor)))
                              (range n-major)))]
    {:vertices verts :indices indices}))

(defn- box-mesh
  "Small axis-aligned box, half-extents `[hx hy hz]`, 8 shared vertices with
   vertex-averaged (not per-face) normals — a legitimate simplification for a
   tiny accessory bridge/pendant where flat-vs-smooth shading is not visually
   significant at this scale, not worth 24-vertex per-face-normal geometry."
  [[hx hy hz]]
  (let [corners (vec (for [sx [-1 1] sy [-1 1] sz [-1 1]]
                        [(* sx hx) (* sy hy) (* sz hz)]))
        vertices (mapv (fn [[x y z]] {:position [x y z] :normal (v3-norm [x y z]) :uv [0.0 0.0]}) corners)
        ;; corners indexed sx,sy,sz in {0,1} order via bit i = sx*4+sy*2+sz
        i (fn [sx sy sz] (+ (* sx 4) (* sy 2) sz))
        faces [[(i 0 0 0) (i 1 0 0) (i 1 1 0) (i 0 1 0)]   ;; -z
               [(i 0 0 1) (i 0 1 1) (i 1 1 1) (i 1 0 1)]   ;; +z
               [(i 0 0 0) (i 0 1 0) (i 0 1 1) (i 0 0 1)]   ;; -x
               [(i 1 0 0) (i 1 0 1) (i 1 1 1) (i 1 1 0)]   ;; +x
               [(i 0 0 0) (i 0 0 1) (i 1 0 1) (i 1 0 0)]   ;; -y
               [(i 0 1 0) (i 1 1 0) (i 1 1 1) (i 0 1 1)]]  ;; +y
        indices (vec (mapcat (fn [[a b c d]] [a b c a c d]) faces))]
    {:vertices vertices :indices indices}))

(defn- quad-mesh
  "A flat `2*hw` x `2*hh` quad in local XY, facing local +Z. Used for decal
   patches — a rotated-to-`normal` copy, not a real UV-decal projection."
  [hw hh]
  {:vertices [{:position [(- hw) (- hh) 0.0] :normal [0.0 0.0 1.0] :uv [0.0 0.0]}
              {:position [hw (- hh) 0.0] :normal [0.0 0.0 1.0] :uv [1.0 0.0]}
              {:position [hw hh 0.0] :normal [0.0 0.0 1.0] :uv [1.0 1.0]}
              {:position [(- hw) hh 0.0] :normal [0.0 0.0 1.0] :uv [0.0 1.0]}]
   :indices [0 1 2 0 2 3]})

(defn- orient-to-normal
  "Rotate `mesh`'s (local +Z-facing) vertices so +Z maps to `n`, then
   translate to `origin`. Builds an orthonormal basis from `n` directly
   (Gram-Schmidt against a non-parallel reference axis) rather than a full
   quaternion — this is a one-off placement, not an animated transform."
  [{:keys [vertices indices]} n origin]
  (let [n (v3-norm n)
        ref (if (> (Math/abs (nth n 1)) 0.9) [1.0 0.0 0.0] [0.0 1.0 0.0])
        t (v3-norm (v3-cross ref n))
        b (v3-cross n t)]
    {:vertices (mapv (fn [{:keys [position normal]}]
                        (let [[px py pz] position [nx ny nz] normal
                              tx-p (v3+ (v3+ (v3* t px) (v3* b py)) (v3* n pz))
                              tx-n (v3+ (v3+ (v3* t nx) (v3* b ny)) (v3* n nz))]
                          {:position (v3+ tx-p origin) :normal tx-n :uv [0.0 0.0]}))
                      vertices)
     :indices indices}))

(defn- offset-part [{:keys [vertices indices]} origin]
  {:vertices (mapv (fn [v] (update v :position v3+ origin)) vertices) :indices indices})

;; ── accessory catalog ──────────────────────────────────────────────────

(def accessory-catalog
  "id -> `{:label :attach-bone :offset :geometry-fn :base-color}`.
   `:attach-bone` is a real `generate-humanoid-skeleton` bone name;
   `:offset` is head/bone-local `[x y z]` (metres) added to that bone's
   rest-pose world position (via `character.body/bone-world-positions`,
   the same positioning scheme `character-creator.pipeline` already uses
   for every other part — no second scheme invented here). Anchors reuse
   this session's established head-local conventions where applicable
   (eyes x=+-0.032 y=0.045 from the blendshape work)."
  {:glasses-round
   {:label "Round glasses" :attach-bone "head"
    :parts (fn [] [{:mesh (torus-mesh 0.018 0.003 20 10) :offset [0.032 -0.005 0.075]}
                   {:mesh (torus-mesh 0.018 0.003 20 10) :offset [-0.032 -0.005 0.075]}
                   {:mesh (box-mesh [0.014 0.002 0.002]) :offset [0.0 -0.005 0.078]}])
    :base-color [0.08 0.08 0.09 1.0]}
   :cap-simple
   {:label "Cap" :attach-bone "head"
    :parts (fn [] [{:mesh (sphere-mesh 0.14 8 16) :offset [0.0 0.04 -0.01]}])
    :base-color [0.2 0.4 0.7 1.0]}
   :earring-stud
   {:label "Earrings" :attach-bone "head"
    :parts (fn [] [{:mesh (sphere-mesh 0.006 6 8) :offset [0.10 0.0 0.0]}
                   {:mesh (sphere-mesh 0.006 6 8) :offset [-0.10 0.0 0.0]}])
    :base-color [0.9 0.75 0.2 1.0]}
   :necklace-simple
   {:label "Necklace" :attach-bone "upperChest"
    :parts (fn [] [{:mesh (torus-mesh 0.06 0.003 24 8) :offset [0.0 -0.01 0.05]}
                   {:mesh (sphere-mesh 0.012 8 10) :offset [0.0 -0.07 0.05]}])
    :base-color [0.85 0.7 0.15 1.0]}})

(defn generate-accessory-part
  "`id` (a key of `accessory-catalog`) + `bone-world-pos-by-name` (`{bone-
   name [x y z]}`, from `character.body/bone-world-positions` zipped with
   bone names) -> a `MeshPart` (`{:name :vertices :indices :material}`,
   `:material` = `id` itself — `character-creator.pipeline` extends its
   material array with `accessory-catalog`'s `:base-color`s under these
   same keys, see its own doc). Returns `nil` for an unknown id (a display-
   layer convenience, mirrors `gpu-adapter/mesh-geometry-by-name`'s nil-
   for-missing convention) rather than throwing."
  [id bone-world-pos-by-name]
  (when-let [{:keys [attach-bone parts]} (get accessory-catalog id)]
    (let [origin (get bone-world-pos-by-name attach-bone [0.0 0.0 0.0])
          merged (reduce (fn [{v1 :vertices i1 :indices} {:keys [mesh offset]}]
                            (let [{v2 :vertices i2 :indices} (offset-part mesh (v3+ origin offset))
                                  base (count v1)]
                              {:vertices (into v1 v2) :indices (into i1 (map #(+ % base) i2))}))
                          {:vertices [] :indices []}
                          (parts))]
      (assoc merged :name (name id) :material id))))

;; ── decal catalog (see namespace docstring — NOT real texture decals) ────

(def decal-catalog
  "id -> `{:label :attach-bone :offset :normal :half-size :base-color}`.
   `:normal` is a fixed, hand-chosen outward-facing direction (world-space-
   ish, since these limbs/torso are built along known axes — see each
   entry's comment) — there is no generic per-vertex surface-normal lookup
   at an arbitrary anchor point in this mesh representation, so a manually
   chosen direction is the pragmatic honest approximation, not a bug."
  {:tattoo-arm-band
   {:label "Arm band tattoo" :attach-bone "leftLowerArm"
    :offset [-0.05 0.0 0.0] :normal [0.0 0.0 1.0] :half-size [0.02 0.012]
    :base-color [0.12 0.16 0.35 1.0]}                          ;; wraps the forearm, facing +Z (front)
   :tattoo-shoulder
   {:label "Shoulder tattoo" :attach-bone "leftShoulder"
    :offset [-0.02 -0.01 0.03] :normal [-0.3 0.2 0.9] :half-size [0.025 0.025]
    :base-color [0.1 0.1 0.1 1.0]}                              ;; outward-forward off the shoulder cap
   :scar-cheek
   {:label "Cheek scar" :attach-bone "head"
    :offset [0.06 -0.02 0.06] :normal [0.9 0.0 0.4] :half-size [0.006 0.03]
    :base-color [0.75 0.45 0.42 1.0]}})                         ;; thin vertical streak, outward+forward off the cheek

(defn generate-decal-part
  "Like `generate-accessory-part` but for `decal-catalog` — a single flat
   quad (`orient-to-normal` + `offset-part`) offset a hair's-width (`0.002`)
   outward along `:normal` from the attach point so it doesn't z-fight the
   skin/clothing mesh underneath. Returns `nil` for an unknown id."
  [id bone-world-pos-by-name]
  (when-let [{:keys [attach-bone offset normal half-size]} (get decal-catalog id)]
    (let [origin (v3+ (get bone-world-pos-by-name attach-bone [0.0 0.0 0.0])
                       (v3+ offset (v3* (v3-norm normal) 0.002)))
          [hw hh] half-size
          part (orient-to-normal (quad-mesh hw hh) normal origin)]
      (assoc part :name (name id) :material id))))
