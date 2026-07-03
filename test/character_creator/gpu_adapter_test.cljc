(ns character-creator.gpu-adapter-test
  (:require [clojure.test :refer [deftest is testing]]
            [character-creator.doc :as doc]
            [character-creator.pipeline :as pipeline]
            [character-creator.gpu-adapter :as gpu]
            [character-creator.expression-bridge :as bridge]))

(deftest head-mesh-geometry-shape-test
  (testing "decodes real positions/normals/indices/morph deltas off the in-memory VrmDocument"
    (let [vdoc (pipeline/character-doc->vrm-document (doc/default-character-doc))
          geo (gpu/head-mesh-geometry vdoc)]
      (is (pos? (count (:positions geo))))
      (is (= (count (:positions geo)) (count (:normals geo))))
      (is (pos? (count (:indices geo))))
      (is (zero? (mod (count (:indices geo)) 3)))
      (is (= 52 (count (:morph-target-names geo))))
      (is (= 52 (count (:morph-target-deltas geo))))
      ;; every morph target has one delta per base vertex
      (is (every? #(= (count %) (count (:positions geo))) (:morph-target-deltas geo))))))

(deftest preset-weight-vector-test
  (testing "expands a VRM preset into a full ARKit-indexed weight vector"
    (let [vdoc (pipeline/character-doc->vrm-document (doc/default-character-doc))
          geo (gpu/head-mesh-geometry vdoc)
          w (gpu/preset-weight-vector bridge/preset->arkit-weights (:morph-target-names geo) :happy)]
      (is (= 52 (count w)))
      (is (pos? (count (filter pos? w))))
      (let [blink-idx (first (keep-indexed #(when (= %2 "eyeBlinkLeft") %1) (:morph-target-names geo)))]
        (is (= 0.0 (nth w blink-idx)))))))

(deftest body-mesh-geometry-shape-test
  (testing "decodes real positions/normals/indices/JOINTS_0/WEIGHTS_0 off the skinned body mesh"
    (let [vdoc (pipeline/character-doc->vrm-document (doc/default-character-doc))
          geo (gpu/body-mesh-geometry vdoc)]
      (is (pos? (count (:positions geo))))
      (is (= (count (:positions geo)) (count (:joints geo)) (count (:weights geo))))
      (is (every? #(= 4 (count %)) (:joints geo)))
      (is (every? #(= 4 (count %)) (:weights geo)))
      (is (every? #(< (Math/abs (- (reduce + %) 1.0)) 1e-4) (:weights geo))))))

;; ── :uvs / :material / mesh-base-color-texture (real-VRM-base spike follow-up:
;; kami.webgpu.mesh grew a texture path, so the reader side needs TEXCOORD_0 +
;; the primitive's material index) ────────────────────────────────────────

(deftest mesh-geometry-by-name-carries-uvs-and-material-index-test
  (testing "TEXCOORD_0 (character-creator.gltf-build already writes it) and the primitive's :material index are both surfaced -- needed to resolve a part's texture"
    (let [vdoc (pipeline/character-doc->vrm-document (doc/default-character-doc))
          geo (gpu/body-mesh-geometry vdoc)]
      (is (= (count (:positions geo)) (count (:uvs geo))))
      (is (every? #(= 2 (count %)) (:uvs geo)))
      (is (number? (:material geo))))))

(deftest mesh-geometry-by-name-uvs-absent-for-untextured-parts-test
  (testing "character-creator's own generated parts have no baseColorTexture yet, so mesh-base-color-texture is nil (not an error) -- the shared default-white-texture fallback in kami.webgpu.mesh covers this"
    (let [vdoc (pipeline/character-doc->vrm-document (doc/default-character-doc))]
      (is (nil? (gpu/mesh-base-color-texture vdoc "body")))
      (is (nil? (gpu/mesh-base-color-texture vdoc "nonexistent-mesh"))))))

;; ── multi-primitive meshes (real bug fix, /loop maturity pass): every
;; mesh-* reader used to hardcode `:primitives 0`, silently dropping every
;; primitive after the first. character-creator's own generated parts are
;; all single-primitive so this was invisible against this app's own
;; output -- a real parsed VRM (VRoid Studio's standard export shape)
;; commonly gives a head mesh 2-3 primitives with distinct materials.
;; Synthetic fixture: split a real mesh's sole primitive into two,
;; assigning each a distinct material index, to prove the reader now
;; surfaces both instead of only the first. ─────────────────────────────

(defn- split-mesh-into-two-primitives
  "Test helper: given a real `VrmDocument` and a mesh name whose mesh has
  exactly one primitive, returns a `VrmDocument` where that mesh now has
  TWO primitives -- the original, and a copy of it reassigned to material
  index `alt-material-idx`. Both primitives share the same accessors (same
  geometry data) since this test only cares about primitive COUNT and
  per-primitive :material resolution, not distinct geometry per primitive."
  [vdoc mesh-name alt-material-idx]
  (let [meshes (get-in vdoc [:gltf :meshes])
        mesh-idx (first (keep-indexed (fn [i m] (when (= mesh-name (:name m)) i)) meshes))
        prim (get-in vdoc [:gltf :meshes mesh-idx :primitives 0])
        alt-prim (assoc prim :material alt-material-idx)]
    (update-in vdoc [:gltf :meshes mesh-idx :primitives] conj alt-prim)))

(deftest mesh-primitives-by-name-reads-every-primitive-test
  (testing "a real bug: mesh-geometry-by-name used to hardcode :primitives 0, dropping every other primitive -- mesh-primitives-by-name now returns all of them"
    (let [vdoc0 (pipeline/character-doc->vrm-document (doc/default-character-doc))
          orig-material (:material (gpu/mesh-geometry-by-name vdoc0 "body"))
          alt-material (if (zero? orig-material) 1 0) ;; any distinct index -- this doc's :materials count is >= 2
          vdoc (split-mesh-into-two-primitives vdoc0 "body" alt-material)
          prims (gpu/mesh-primitives-by-name vdoc "body")]
      (is (= 2 (count prims)))
      (is (= orig-material (:material (nth prims 0))))
      (is (= alt-material (:material (nth prims 1))))
      ;; both primitives decode real, non-empty geometry (not just the first)
      (is (every? #(pos? (count (:positions %))) prims))
      (is (every? #(pos? (count (:indices %))) prims)))))

(deftest mesh-geometry-by-name-still-returns-only-first-primitive-test
  (testing "mesh-geometry-by-name stays a thin first-primitive wrapper -- backward compatible for every existing single-primitive caller"
    (let [vdoc0 (pipeline/character-doc->vrm-document (doc/default-character-doc))
          orig-material (:material (gpu/mesh-geometry-by-name vdoc0 "body"))
          alt-material (if (zero? orig-material) 1 0)
          vdoc (split-mesh-into-two-primitives vdoc0 "body" alt-material)]
      (is (= orig-material (:material (gpu/mesh-geometry-by-name vdoc "body")))))))

(deftest mesh-primitives-by-name-missing-mesh-returns-empty-vec-test
  (testing "no mesh with that name -> [] (mirrors mesh-geometry-by-name's nil-for-missing convention)"
    (let [vdoc (pipeline/character-doc->vrm-document (doc/default-character-doc))]
      (is (= [] (gpu/mesh-primitives-by-name vdoc "nonexistent-mesh"))))))

(deftest mesh-primitives-by-index-test
  (testing "resolves a mesh by raw index (for an uploaded VRM whose mesh names can't be relied on), same shape as mesh-primitives-by-name"
    (let [vdoc0 (pipeline/character-doc->vrm-document (doc/default-character-doc))
          body-idx (first (keep-indexed (fn [i m] (when (= "body" (:name m)) i)) (get-in vdoc0 [:gltf :meshes])))
          by-name (gpu/mesh-primitives-by-name vdoc0 "body")
          by-idx (gpu/mesh-primitives-by-index vdoc0 body-idx)]
      (is (= by-name by-idx))
      (is (= [] (gpu/mesh-primitives-by-index vdoc0 nil)))
      (is (= [] (gpu/mesh-primitives-by-index vdoc0 -1)))
      (is (= [] (gpu/mesh-primitives-by-index vdoc0 99999))))))

(deftest material-base-color-texture-nil-material-idx-test
  (testing "material-base-color-texture takes a material index directly (for per-primitive resolution) and is nil-safe"
    (let [vdoc (pipeline/character-doc->vrm-document (doc/default-character-doc))]
      (is (nil? (gpu/material-base-color-texture vdoc nil)))
      ;; character-creator's own materials have no embedded baseColorTexture yet
      (is (nil? (gpu/material-base-color-texture vdoc 0))))))
