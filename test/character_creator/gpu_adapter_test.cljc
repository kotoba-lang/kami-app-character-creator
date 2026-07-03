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
