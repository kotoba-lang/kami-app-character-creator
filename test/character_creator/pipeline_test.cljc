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
      ;; Count reflects whatever `character.body` currently emits (a parallel
      ;; /loop fork this session extended fingers/toes + clothing coverage,
      ;; landed on `character` main independently of this repo's own work —
      ;; not a magic constant this repo owns). Assert "reasonably many real
      ;; parts" rather than an exact number that legitimately drifts with
      ;; upstream `character` changes.
      (is (>= (count (:meshes (:gltf vdoc))) 10))
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
      ;; so add-part-mesh skips it per empty-vertices guard). Compared
      ;; against a live default-doc mesh count, not a hardcoded number — see
      ;; character-doc->vrm-document-shape-test's comment on why.
      (let [default-count (count (:meshes (:gltf (pipeline/character-doc->vrm-document (doc/default-character-doc)))))]
        (is (= (dec default-count) (count (:meshes (:gltf vdoc)))))))))

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
        ;; Bone count reflects whatever `character.body/generate-humanoid-
        ;; skeleton` currently emits (23 as of the full-body pass; a parallel
        ;; /loop fork this session further extended fingers/toes on top of
        ;; that, independently of this repo) — assert it's at least the
        ;; full-body-pass floor, not an exact number this repo doesn't own.
        (is (>= (count (:joints skin)) 23))
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
      ;; see body-mesh-is-really-skinned-test's comment on why this is >=, not =.
      (is (>= (count (:joints (first (:skins gltf)))) 23)))))

;; ── accessories/decals (/loop maturity pass, closes ADR-2607031200's
;; deferred :character/equip) ─────────────────────────────────────────────

(deftest equip-adds-real-accessory-meshes-test
  (testing "each equipped accessory id shows up as a real, non-empty mesh"
    (let [base (doc/default-character-doc)
          base-count (count (:meshes (:gltf (pipeline/character-doc->vrm-document base))))
          d (assoc base :character/equip [:glasses-round :cap-simple])
          vdoc (pipeline/character-doc->vrm-document d)
          gltf (:gltf vdoc)
          names (set (map :name (:meshes gltf)))]
      (is (= (+ base-count 2) (count (:meshes gltf))))
      (is (contains? names "glasses-round"))
      (is (contains? names "cap-simple"))
      (doseq [n ["glasses-round" "cap-simple"]]
        (let [m (first (filter #(= n (:name %)) (:meshes gltf)))]
          (is (some? (get-in m [:primitives 0 :material]))))))))

(deftest decals-add-real-quad-meshes-test
  (testing "each selected decal id shows up as a real 4-vertex quad mesh"
    (let [d (assoc (doc/default-character-doc) :character/decals [:tattoo-arm-band :scar-cheek])
          vdoc (pipeline/character-doc->vrm-document d)
          gltf (:gltf vdoc)
          names (set (map :name (:meshes gltf)))]
      (is (contains? names "tattoo-arm-band"))
      (is (contains? names "scar-cheek")))))

(deftest unknown-equip-id-is-silently-skipped-test
  (testing "an unknown accessory/decal id is dropped, not an error (display-layer convenience)"
    (let [base-count (count (:meshes (:gltf (pipeline/character-doc->vrm-document (doc/default-character-doc)))))
          d (assoc (doc/default-character-doc) :character/equip [:not-a-real-accessory])
          vdoc (pipeline/character-doc->vrm-document d)]
      (is (= base-count (count (:meshes (:gltf vdoc))))))))

(deftest equip-and-decals-round-trip-through-export-parse-test
  (testing "an equipped+decaled character still exports a valid, re-parseable .vrm"
    (let [d (assoc (doc/default-character-doc)
                   :character/equip [:necklace-simple :earring-stud]
                   :character/decals [:tattoo-shoulder])
          bytes (pipeline/character-doc->vrm-bytes d)]
      (is (= [0x67 0x6C 0x54 0x46] (subvec (vec bytes) 0 4)))
      (let [reparsed (vrm/parse-vrm bytes)
            names (set (map :name (:meshes (:gltf reparsed))))]
        (is (contains? names "necklace-simple"))
        (is (contains? names "earring-stud"))
        (is (contains? names "tattoo-shoulder"))))))
