(ns character-creator.pattern-demo
  "/loop maturity pass (ADR-2607031200) browser proof for `kami.webgpu.mesh`'s
  new procedural-pattern uniform (`color_b`/`pattern_kind`/`pattern_params` —
  see that namespace's docstring). Draws a quad twice, side by side: left
  with `pattern` explicitly `nil` (forces `pattern_kind` back to its default
  `0` — reproduces the old flat-`:base-color`-only look byte-for-byte),
  right with the real `character-creator.accessories/decal-catalog`
  `:scar-cheek` colours (`:base-color` -> `:pattern`'s `:fade-to`, a radial
  gradient) — the one shader, one mesh, pattern-vs-no-pattern comparison a
  screenshot can prove.

  Camera-facing synthetic quad, not the real decal's own (tilted, oriented-
  to-a-hand-picked-normal, very thin — half-size [0.006 0.03]) geometry: an
  early version drew the actual `generate-decal-part` quad and it rendered
  as a near-edge-on sliver from this demo's fixed camera (real bug, caught
  by looking at the actual screenshot, not assumed) — the exact placement/
  orientation is already covered by `accessories_test.cljc`'s unit tests, so
  this demo's OWN job is just to prove the shader/uniform path renders a
  real gradient, which needs a plain camera-facing square, not a faithful
  in-character reproduction. The COLOUR data is still 100% real, read
  straight from `decal-catalog` — only the quad's own shape/orientation and
  the pattern's centre/radius (fit to this synthetic quad's own extent, not
  re-derived from a bone position) are demo-only stand-ins."
  (:require [character-creator.accessories :as acc]
            [kami.webgpu.mesh :as mesh]))

(defn- face-quad
  "A flat square, `side` across, centred at the origin, facing +Z (normal
  `[0 0 1]`) — a plain, camera-facing stand-in for a pattern demo (see
  namespace docstring for why this isn't the real decal geometry)."
  [side]
  (let [h (/ side 2.0)
        n [0.0 0.0 1.0]
        p1 [(- h) (- h) 0.0] p2 [h (- h) 0.0] p3 [h h 0.0] p4 [(- h) h 0.0]]
    {:positions [p1 p2 p3 p4]
     :normals [n n n n]
     :indices [0 1 2 0 2 3]}))

(defn- model-translate [[tx ty tz]]
  (js/Float32Array. #js [1 0 0 0, 0 1 0 0, 0 0 1 0, tx ty tz 1]))

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

                    quad (face-quad 1.0)
                    ;; `kami.webgpu.mesh` allocates ONE uniform buffer per
                    ;; `upload-mesh!` call, written fresh by every `draw!` —
                    ;; calling `draw!` twice against the SAME `buffers` (as
                    ;; an earlier version of this demo did) queues two
                    ;; `writeBuffer`s to the same uniform buffer before
                    ;; `.submit()`, so the second silently clobbers the
                    ;; first and both draws render identically (a real
                    ;; latent limitation of that executor's one-gbuf-per-
                    ;; mesh design, caught here, not a bug in the new
                    ;; pattern code) — upload the same geometry TWICE,
                    ;; matching how `character-creator.app` already keeps
                    ;; one `upload-mesh!` result per simultaneously-drawn
                    ;; mesh instance.
                    left-buffers (mesh/upload-mesh! mctx quad)
                    right-buffers (mesh/upload-mesh! mctx quad)
                    ;; real colours, straight from the catalog — only the quad
                    ;; shape above and this pattern's centre/radius (fit to
                    ;; THIS quad, not the real decal's own placement) are
                    ;; demo stand-ins, per the namespace docstring.
                    {:keys [base-color pattern]} (:scar-cheek acc/decal-catalog)
                    color3 (subvec (vec base-color) 0 3)
                    real-pattern {:color-b (:fade-to pattern) :kind 2 :params [0.0 0.0 0.0 0.5]}]
                (log "quad vertices:" (count (:positions quad)) "pattern:" (pr-str real-pattern))
                (set! (.-innerText (js/document.getElementById "status")) "WebGPU OK — left: flat, right: radial-gradient pattern.")
                (set! (.-__kami_pattern_ready js/window) true)
                ((fn tick []
                   (js/requestAnimationFrame
                     (fn [_]
                       (let [vp (mesh/view-projection [0 0 2.4] [0 0 0] (/ w (max 1 h)))
                             left-mvp (m4-mul vp (model-translate [-0.65 0 0]))
                             right-mvp (m4-mul vp (model-translate [0.65 0 0]))
                             enc (.createCommandEncoder device)
                             view (.createView (.getCurrentTexture ctx))
                             pass (.beginRenderPass enc
                                    #js {:colorAttachments #js [#js {:view view :loadOp "clear" :storeOp "store"
                                                                     :clearValue #js {:r 0.12 :g 0.12 :b 0.14 :a 1}}]
                                         :depthStencilAttachment #js {:view (.createView depth-tex) :depthLoadOp "clear"
                                                                      :depthStoreOp "store" :depthClearValue 1.0}})]
                         ;; left: pattern nil -> mesh/draw!'s 7-arity clause, or an explicit
                         ;; nil 8th arg -> :kind defaults to 0 (flat) either way; use the
                         ;; explicit nil to prove the SAME 8-arity call path also reproduces
                         ;; flat correctly, not just the old 7-arity call sites.
                         (mesh/draw! mctx pass left-buffers left-mvp color3 [] [] nil)
                         (mesh/draw! mctx pass right-buffers right-mvp color3 [] [] real-pattern)
                         (.end pass)
                         (.submit (.-queue device) #js [(.finish enc)])
                         (tick)))))))))
          (.catch (fn [err] (log "WebGPU init failed:" err)
                    (set! (.-innerText (js/document.getElementById "status")) (str "WebGPU init failed: " err))))))))

(main)
