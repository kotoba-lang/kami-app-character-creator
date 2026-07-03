(ns character-creator.pipeline-test
  (:require [clojure.test :refer [deftest is testing]]
            [vrm]
            [vrm.glb :as glb]
            [character-creator.doc :as doc]
            [character-creator.pipeline :as pipeline]))

(deftest character-doc->vrm-document-shape-test
  (testing "produces a well-formed VrmDocument"
    (let [d (doc/default-character-doc)
          vdoc (pipeline/character-doc->vrm-document d)]
      (is (= :v1-0 (:version vdoc)))
      (is (pos? (count (:meshes (:gltf vdoc)))))
      ;; head + 6 eye parts (2 sides x white/iris/pupil) + hair + body + clothing = 10
      (is (= 10 (count (:meshes (:gltf vdoc)))))
      (is (pos? (count (:human-bones (:humanoid vdoc)))))
      (is (= 18 (count (:expressions vdoc))))
      (is (some #(= :hips (:bone %)) (:human-bones (:humanoid vdoc))))
      (is (some #(= :head (:bone %)) (:human-bones (:humanoid vdoc)))))))

(deftest character-doc->vrm-bytes-is-valid-glb-test
  (testing "export-glb output starts with the GLB magic header and round-trips through parse-glb"
    (let [d (doc/default-character-doc)
          bytes (pipeline/character-doc->vrm-bytes d)]
      (is (pos? (count bytes)))
      ;; "glTF" magic, little-endian u32, as 4 bytes: 0x67 0x6C 0x54 0x46
      (is (= [0x67 0x6C 0x54 0x46] (subvec (vec bytes) 0 4)))
      (let [chunks (glb/parse-glb bytes)]
        (is (seq (:json chunks)))
        (is (seq (:bin chunks)))))))

(deftest character-doc->vrm-bytes-reparses-test
  (testing "vrm/parse-vrm can re-read what this pipeline exports (full round-trip)"
    (let [d (doc/default-character-doc)
          bytes (pipeline/character-doc->vrm-bytes d)
          reparsed (vrm/parse-vrm bytes)]
      (is (= :v1-0 (:version reparsed)))
      (is (= (:character/name d) (:name (:meta reparsed))))
      (is (pos? (count (:human-bones (:humanoid reparsed)))))
      (is (= 18 (count (:expressions reparsed)))))))

(deftest boot-config-flows-through-pipeline-test
  (testing "a boot-config'd CharacterDoc (different hair/clothing) still exports cleanly"
    (let [d (doc/boot-config {:name "Buzz Test" :hair-preset :buzz :clothing-preset :hoodie})
          bytes (pipeline/character-doc->vrm-bytes d)]
      (is (= [0x67 0x6C 0x54 0x46] (subvec (vec bytes) 0 4)))
      (is (= "Buzz Test" (:name (:meta (vrm/parse-vrm bytes))))))))

(deftest bald-hair-produces-no-empty-mesh-test
  (testing ":bald hair (0 vertices) is skipped, not written as a degenerate mesh"
    (let [d (doc/boot-config {:hair-preset :bald})
          vdoc (pipeline/character-doc->vrm-document d)]
      ;; one fewer mesh than the default (no hair mesh: :bald has 0 strands,
      ;; so add-part-mesh skips it per empty-vertices guard)
      (is (= 9 (count (:meshes (:gltf vdoc))))))))

;; ── skinning (/loop maturity pass, ADR-2607031200) ───────────────────────

(deftest body-mesh-is-really-skinned-test
  (testing "the body mesh's primitive carries JOINTS_0/WEIGHTS_0 and a real :skins entry"
    (let [d (doc/default-character-doc)
          vdoc (pipeline/character-doc->vrm-document d)
          gltf (:gltf vdoc)
          body-mesh (first (filter #(= "body" (:name %)) (:meshes gltf)))
          attrs (:attributes (first (:primitives body-mesh)))]
      (is (some? body-mesh))
      (is (contains? attrs :JOINTS_0))
      (is (contains? attrs :WEIGHTS_0))
      (is (= 1 (count (:skins gltf))))
      (let [skin (first (:skins gltf))]
        ;; 23 bones: the original 13 core + 10 added for the full-body
        ;; extension (/loop maturity pass, ADR-2607031200 follow-up).
        (is (= 23 (count (:joints skin))))
        (is (some? (:inverseBindMatrices skin))))
      ;; the body mesh's node references the skin.
      (let [mesh-idx (.indexOf (:meshes gltf) body-mesh)
            node (first (filter #(= mesh-idx (:mesh %)) (:nodes gltf)))]
        (is (= 0 (:skin node)))))))

(deftest body-mesh-round-trips-skin-through-export-parse-test
  (testing "vrm/parse-vrm re-reads the exported skin (not just the raw builder state)"
    (let [d (doc/default-character-doc)
          bytes (pipeline/character-doc->vrm-bytes d)
          reparsed (vrm/parse-vrm bytes)
          gltf (:gltf reparsed)]
      (is (= 1 (count (:skins gltf))))
      (is (= 23 (count (:joints (first (:skins gltf)))))))))
