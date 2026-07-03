(ns character-creator.preview-demo
  "Phase 2 (ADR-2607031200) browser proof: a real character-creator head mesh
  morphing between VRM expressions, and a synthetic 2-bone skinned strip
  bending — both drawn through `kami.webgpu.mesh` (kotoba-lang/webgpu), the new
  skin/morph WebGPU executor. Dev-only entry point (`:cljs` alias), not part of
  the portable pipeline. See `kami.webgpu.mesh`'s docstring for why this is a
  new sibling executor rather than a `kami.webgpu.cljs` edit, and why skinning
  is demoed on a synthetic fixture rather than a real avatar."
  (:require [character-creator.doc :as doc]
            [character-creator.pipeline :as pipeline]
            [character-creator.gpu-adapter :as gpu]
            [character-creator.expression-bridge :as bridge]
            [kami.webgpu.mesh :as mesh]))

;; --- bounding-box fit: normalize the real head mesh into a unit-ish cube so
;; it's visible regardless of character.body's native coordinate scale -------

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

;; --- synthetic 2-bone bending strip (skinning proof — Phase 1's real avatar
;; has no JOINTS_0/WEIGHTS_0, per character.body's documented limitation) ----

(defn- synthetic-arm []
  (let [rings (vec (map #(* % 0.2) (range 11)))
        ring-pts (fn [x] (mapv (fn [a] (let [ang (* a (/ js/Math.PI 2.0))]
                                          [x (* 0.15 (js/Math.cos ang)) (* 0.15 (js/Math.sin ang))]))
                               (range 4)))
        positions (vec (mapcat ring-pts rings))
        normals (mapv (fn [[_ y z]] (let [l (max (js/Math.hypot y z) 1e-6)] [0.0 (/ y l) (/ z l)])) positions)
        weight1 (fn [x] (max 0.0 (min 1.0 (/ (- x 0.7) 0.6))))
        joints (mapv (fn [_] [0 1 0 0]) positions)
        weights (mapv (fn [[x _ _]] (let [w1 (weight1 x)] [(- 1.0 w1) w1 0.0 0.0])) positions)
        idx-of (fn [ring pt] (+ (* ring 4) pt))
        indices (vec (mapcat (fn [r]
                                (mapcat (fn [p]
                                          (let [p2 (mod (inc p) 4)]
                                            [(idx-of r p) (idx-of (inc r) p) (idx-of r p2)
                                             (idx-of r p2) (idx-of (inc r) p) (idx-of (inc r) p2)]))
                                        (range 4)))
                              (range (dec (count rings)))))]
    {:positions positions :normals normals :indices indices :joints joints :weights weights}))

;; --- tiny model-matrix helper (translate only; the mesh data is already
;; unit-scaled) -----------------------------------------------------------

(defn- model-translate [[x y z]]
  (js/Float32Array. #js [1 0 0 0, 0 1 0 0, 0 0 1 0, x y z 1]))

(defn- m4-mul [a b]
  (let [o (js/Float32Array. 16)]
    (dotimes [c 4]
      (dotimes [r 4]
        (aset o (+ (* c 4) r)
              (+ (* (aget a r) (aget b (+ (* c 4) 0)))
                 (* (aget a (+ r 4)) (aget b (+ (* c 4) 1)))
                 (* (aget a (+ r 8)) (aget b (+ (* c 4) 2)))
                 (* (aget a (+ r 12)) (aget b (+ (* c 4) 3)))))))
    o))

;; --- 2-bone joint matrices for a bend angle theta (radians) around the pivot
;; at x=1.0 -------------------------------------------------------------------

(defn- arm-joint-matrices [theta]
  (let [c (js/Math.cos theta) s (js/Math.sin theta)
        piv 1.0
        ;; R(theta) about Z axis, then translate so the pivot (piv,0,0) stays fixed:
        ;; M = T(piv) . R . T(-piv)  →  translation term = piv - R*piv
        rot #js [c s 0 0, (- s) c 0 0, 0 0 1 0, 0 0 0 1]
        tx (- piv (* c piv))
        ty (* s piv)]
    (aset rot 12 tx) (aset rot 13 ty)
    [(js/Float32Array. #js [1 0 0 0, 0 1 0 0, 0 0 1 0, 0 0 0 1]) ;; joint 0: identity (upper arm fixed)
     (vec rot)]))

(defn- log [& xs] (.apply (.-log js/console) js/console (clj->js xs)))

(defn ^:export main []
  (let [canvas (js/document.getElementById "gpu-canvas")]
    (if-not (.-gpu js/navigator)
      (do (log "WebGPU not available (navigator.gpu is undefined)")
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

                    ;; --- real character-creator head mesh (real geometry; SYNTHETIC morph
                    ;; delta — see below) ---
                    vdoc (pipeline/character-doc->vrm-document (doc/default-character-doc))
                    geo (gpu/head-mesh-geometry vdoc)
                    fit (bbox-fit (:positions geo))
                    ;; FINDING (not a Phase 2 bug — verified at the character.blendshape source):
                    ;; character.blendshape/generate-arkit-targets is a documented placeholder —
                    ;; "zero placeholder — populated from captured data or procedural rules" — so
                    ;; ALL 52 ARKit targets are all-[0 0 0] for every CharacterDef today. Confirmed
                    ;; by injecting a synthetic weight=8.0 on target 0 and diffing screenshots
                    ;; (byte-identical to weight=0). The glTF-morph-target *plumbing* (gltf-build's
                    ;; writer, gpu-adapter's reader, kami.webgpu.mesh's storage-buffer blend shader)
                    ;; is real and correct — proven below by substituting one SYNTHETIC "puff along
                    ;; normal" delta for real target 0, since character.blendshape can't supply a
                    ;; non-zero one yet. character.blendshape itself is untouched (content-authoring
                    ;; work, not a bug fix, is out of this ADR's scope).
                    synthetic-puff (mapv (fn [[nx ny nz]] [(* nx 0.35) (* ny 0.35) (* nz 0.35)]) (:normals geo))
                    morph-deltas (assoc (vec (fit-deltas (:morph-target-deltas geo) (:scale fit)))
                                        0 synthetic-puff)
                    head-buffers (mesh/upload-mesh!
                                   mctx {:positions (fit-positions (:positions geo) fit)
                                         :normals (:normals geo)
                                         :indices (:indices geo)
                                         :morph-target-deltas morph-deltas})

                    ;; --- synthetic 2-bone arm (skinning proof) ---
                    arm-geo (synthetic-arm)
                    arm-buffers (mesh/upload-mesh! mctx arm-geo)]
                (log "head mesh vertices:" (count (:positions geo)) "morph targets:" (count (:morph-target-deltas geo)))
                (log "arm mesh vertices:" (count (:positions arm-geo)))
                (set! (.-innerText (js/document.getElementById "status")) "WebGPU OK — rendering.")
                ((fn tick [t0]
                   (js/requestAnimationFrame
                     (fn [t]
                       (let [tt (/ t 1000.0)
                             vp (mesh/view-projection [0 0.3 2.6] [0 0 0] (/ w (max 1 h)))
                             head-mvp (m4-mul vp (model-translate [-0.8 0 0]))
                             arm-mvp (m4-mul vp (model-translate [0.3 -0.2 0]))
                             ;; index-0 target is the synthetic puff (see above); oscillate its
                             ;; weight 0..1 so the morph is visibly animated, not just present.
                             puff-w (max 0.0 (js/Math.sin (* tt 1.1)))
                             weights (assoc (vec (repeat (count (:morph-target-names geo)) 0.0)) 0 puff-w)
                             theta (* 0.8 (js/Math.sin (* tt 1.3)))
                             joints (arm-joint-matrices theta)
                             enc (.createCommandEncoder device)
                             view (.createView (.getCurrentTexture ctx))
                             pass (.beginRenderPass enc
                                    #js {:colorAttachments #js [#js {:view view :loadOp "clear" :storeOp "store"
                                                                     :clearValue #js {:r 0.08 :g 0.08 :b 0.1 :a 1}}]
                                         :depthStencilAttachment #js {:view (.createView depth-tex) :depthLoadOp "clear"
                                                                      :depthStoreOp "store" :depthClearValue 1.0}})]
                         (mesh/draw! mctx pass head-buffers head-mvp [0.85 0.75 0.65] weights [])
                         (mesh/draw! mctx pass arm-buffers arm-mvp [0.6 0.8 0.95] [] joints)
                         (.end pass)
                         (.submit (.-queue device) #js [(.finish enc)])
                         (tick t)))))
                 0))))
          (.catch (fn [err] (log "WebGPU init failed:" err)
                    (set! (.-innerText (js/document.getElementById "status")) (str "WebGPU init failed: " err))))))))

(main)
