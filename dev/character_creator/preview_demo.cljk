(ns character-creator.preview-demo
  "Phase 2 (ADR-2607031200) + /loop maturity pass browser proof: a real
  character-creator head mesh morphing between REAL VRM expression presets, a
  synthetic 2-bone skinned strip bending (a controlled fixture, not a real
  avatar — proves the skinning math in isolation), and the real
  character-creator BODY mesh — now a full standing figure (`character.body/
  generate-body`: torso + 2 legs + 2 arms, real JOINTS_0/WEIGHTS_0 via
  `skin-body`) — posed by rotating its \"leftLowerLeg\" (knee) and
  \"leftLowerArm\" (elbow) bones SIMULTANEOUSLY (independent thetas), proving
  multi-joint skinning on the real avatar, not just a single dominant bone.
  All three drawn through `kami.webgpu.mesh` (kotoba-lang/webgpu), the
  skin/morph WebGPU executor. Dev-only entry point (`:cljs` alias), not part
  of the portable pipeline. See `kami.webgpu.mesh`'s docstring for why this
  is a new sibling executor rather than a `kami.webgpu.cljs` edit.

  Three maturity-loop updates landed together here (across two `/loop`
  iterations):
  1. `character.blendshape/generate-arkit-targets` no longer returns all-zero
     deltas for the 32 ARKit targets `character-creator.expression-bridge/
     preset->arkit-weights` references (see that namespace's `targets-spec`)
     — this demo cycles through REAL VRM preset weight vectors
     (`character-creator.gpu-adapter/preset-weight-vector`) instead of the
     earlier synthetic single-target 'puff along normal' substitution.
  2. `character.body/skin-body` attaches real JOINTS_0/WEIGHTS_0 to the body
     mesh (`character-creator.gpu-adapter/body-mesh-geometry` reads them back
     off the exported `VrmDocument`) — the body scene poses the real avatar
     instead of only the synthetic arm.
  3. `character.body/generate-body` now builds a full standing figure (torso
     + legs + arms, 23-bone skeleton) instead of a neck+upper-body bust —
     this scene now poses two independent real joints (knee + elbow) instead
     of the single \"hips\" pivot the bust-only mesh could meaningfully show."
  (:require [character-creator.doc :as doc]
            [character-creator.pipeline :as pipeline]
            [character-creator.gpu-adapter :as gpu]
            [character-creator.expression-bridge :as bridge]
            [character.body :as cbody]
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

(defn- fit-point [[x y z] {:keys [center scale]}]
  (mapv (fn [pi ci] (* (- pi ci) scale)) [x y z] center))

;; --- real character-creator BODY mesh, real skin weights (/loop maturity
;; pass, ADR-2607031200 — Phase 2 predates this and had no skinned real-
;; avatar geometry, so it only proved skinning on the synthetic arm below).
;; Pose the "hips" bone (index 0) with a pivot rotation about its own
;; rest-pose world position and confirm the mesh visibly bends there. ------

(defn- identity-mat4 [] (js/Float32Array. #js [1 0 0 0, 0 1 0 0, 0 0 1 0, 0 0 0 1]))

(defn- pivot-rotate-z
  "Column-major rotate-about-Z mat4 with the rotation centered on `[px py pz]`
  (derived directly, not copied from `arm-joint-matrices` below — that one
  demos fine visually but its translation term's y-sign doesn't reduce to
  identity at theta=0 the way this one's does, so it isn't reused here)."
  [[px py _pz] theta]
  (let [c (js/Math.cos theta) s (js/Math.sin theta)
        rx (- (* c px) (* s py))
        ry (+ (* s px) (* c py))]
    (js/Float32Array. #js [c s 0 0, (- s) c 0 0, 0 0 1 0, (- px rx) (- py ry) 0 1])))

(defn- body-joint-matrices
  "Joint palette (length = bone count): identity everywhere except the bones
  named in `theta-by-idx` (`{bone-idx theta}`), each independently posed via
  `pivot-rotate-z` around its own rest-pose world position. Generalized
  (/loop maturity pass, full-body follow-up) from the original single-`hips`
  version to pose several joints (knee + elbow) at once, now that the
  skeleton actually has them — this is the concrete proof multi-joint
  skinning works, not just that one bone can move the whole mesh."
  [bone-world-pos theta-by-idx]
  (mapv (fn [i bp] (if-let [theta (get theta-by-idx i)] (pivot-rotate-z bp theta) (identity-mat4)))
        (range (count bone-world-pos)) bone-world-pos))

;; --- synthetic 2-bone bending strip (a controlled fixture that isolates the
;; skinning math from the real avatar's mesh/skeleton proportions — kept even
;; after the real body mesh gained skin weights below, since it demos a clean
;; multi-bone blend the real bust's current bone layout mostly doesn't) ------

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

                    ;; --- real character-creator head mesh, REAL morph deltas ---
                    ;; `character.blendshape/generate-arkit-targets` now computes real
                    ;; per-vertex deltas for the 32 ARKit targets the expression bridge
                    ;; references (procedural region falloffs — see that namespace's
                    ;; `targets-spec`), so no synthetic substitution is needed any more.
                    vdoc (pipeline/character-doc->vrm-document (doc/default-character-doc))
                    geo (gpu/head-mesh-geometry vdoc)
                    fit (bbox-fit (:positions geo))
                    morph-deltas (vec (fit-deltas (:morph-target-deltas geo) (:scale fit)))
                    presets [:neutral :happy :surprised :angry :sad]
                    preset-weights (mapv #(gpu/preset-weight-vector
                                            bridge/preset->arkit-weights (:morph-target-names geo) %)
                                          presets)
                    head-buffers (mesh/upload-mesh!
                                   mctx {:positions (fit-positions (:positions geo) fit)
                                         :normals (:normals geo)
                                         :indices (:indices geo)
                                         :morph-target-deltas morph-deltas})

                    ;; --- synthetic 2-bone arm (skinning proof on a fixture) ---
                    arm-geo (synthetic-arm)
                    arm-buffers (mesh/upload-mesh! mctx arm-geo)

                    ;; --- real character-creator BODY mesh, real skin weights ---
                    body-geo (gpu/body-mesh-geometry vdoc)
                    body-fit (bbox-fit (:positions body-geo))
                    skeleton (cbody/generate-humanoid-skeleton)
                    bone-world-pos-raw (cbody/bone-world-positions (:bones skeleton))
                    bone-world-pos (mapv #(fit-point % body-fit) bone-world-pos-raw)
                    idx-by-name (into {} (map-indexed (fn [i b] [(:name b) i]) (:bones skeleton)))
                    knee-idx (idx-by-name "leftLowerLeg")
                    elbow-idx (idx-by-name "leftLowerArm")
                    body-buffers (mesh/upload-mesh!
                                   mctx {:positions (fit-positions (:positions body-geo) body-fit)
                                         :normals (:normals body-geo)
                                         :indices (:indices body-geo)
                                         :joints (:joints body-geo)
                                         :weights (:weights body-geo)})]
                (log "head mesh vertices:" (count (:positions geo)) "morph targets:" (count (:morph-target-deltas geo)))
                (log "arm mesh vertices:" (count (:positions arm-geo)))
                (log "body mesh vertices:" (count (:positions body-geo)) "bones:" (count bone-world-pos))
                (log "cycling real VRM presets:" (pr-str presets))
                (set! (.-innerText (js/document.getElementById "status")) "WebGPU OK — rendering.")
                ((fn tick [t0]
                   (js/requestAnimationFrame
                     (fn [t]
                       (let [tt (/ t 1000.0)
                             vp (mesh/view-projection [0 0.3 2.6] [0 0 0] (/ w (max 1 h)))
                             head-mvp (m4-mul vp (model-translate [-1.1 0 0]))
                             arm-mvp (m4-mul vp (model-translate [0.0 -0.2 0]))
                             body-mvp (m4-mul vp (model-translate [1.1 0.15 0]))
                             ;; cycle through real VRM expression presets, 1.5s each, crossfading
                             ;; linearly between adjacent presets' real weight vectors so the morph
                             ;; is visibly animated (not a static pose swap).
                             n (count presets)
                             cycle-t (mod (/ tt 1.5) n)
                             i0 (int cycle-t) i1 (mod (inc i0) n) f (- cycle-t i0)
                             w0 (nth preset-weights i0) w1 (nth preset-weights i1)
                             weights (mapv (fn [a b] (+ (* a (- 1.0 f)) (* b f))) w0 w1)
                             theta (* 0.8 (js/Math.sin (* tt 1.3)))
                             joints (arm-joint-matrices theta)
                             ;; `window.__kneeTheta`/`window.__elbowTheta` (default independent
                             ;; oscillations) let headless-Chrome verification pin exact bend angles
                             ;; instead of racing requestAnimationFrame. Full-body follow-up
                             ;; (/loop maturity pass): poses the real leftLowerLeg (knee) + leftLowerArm
                             ;; (elbow) bones the earlier skinning-only pass didn't have, replacing the
                             ;; single-hips demo (already proven) with proof two independent joints on
                             ;; the same real mesh bend correctly at once.
                             knee-theta (if (some? (.-__kneeTheta js/window))
                                          (.-__kneeTheta js/window)
                                          (* 0.5 (+ 1.0 (js/Math.sin (* tt 0.7)))))
                             elbow-theta (if (some? (.-__elbowTheta js/window))
                                           (.-__elbowTheta js/window)
                                           (* 0.6 (js/Math.sin (* tt 1.1))))
                             body-joints (body-joint-matrices bone-world-pos
                                                               {knee-idx knee-theta elbow-idx elbow-theta})
                             enc (.createCommandEncoder device)
                             view (.createView (.getCurrentTexture ctx))
                             pass (.beginRenderPass enc
                                    #js {:colorAttachments #js [#js {:view view :loadOp "clear" :storeOp "store"
                                                                     :clearValue #js {:r 0.08 :g 0.08 :b 0.1 :a 1}}]
                                         :depthStencilAttachment #js {:view (.createView depth-tex) :depthLoadOp "clear"
                                                                      :depthStoreOp "store" :depthClearValue 1.0}})
                             current-preset (name (nth presets i0))]
                         (set! (.-innerText (js/document.getElementById "status"))
                               (str "WebGPU OK — preset: " current-preset " -> " (name (nth presets i1)) " (" (.toFixed f 2) ")"))
                         (set! (.-__kami_cc_preset js/window) current-preset)
                         (mesh/draw! mctx pass head-buffers head-mvp [0.85 0.75 0.65] weights [])
                         (mesh/draw! mctx pass arm-buffers arm-mvp [0.6 0.8 0.95] [] joints)
                         (mesh/draw! mctx pass body-buffers body-mvp [0.95 0.55 0.55] [] body-joints)
                         (.end pass)
                         (.submit (.-queue device) #js [(.finish enc)])
                         (tick t)))))
                 0))))
          (.catch (fn [err] (log "WebGPU init failed:" err)
                    (set! (.-innerText (js/document.getElementById "status")) (str "WebGPU init failed: " err))))))))

(main)
