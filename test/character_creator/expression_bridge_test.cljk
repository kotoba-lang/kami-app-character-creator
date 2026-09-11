(ns character-creator.expression-bridge-test
  (:require [clojure.test :refer [deftest is testing]]
            [character.blendshape :as blendshape]
            [vrm.vrm-types :as vt]
            [character-creator.expression-bridge :as bridge]))

(deftest covers-all-vrm-presets-test
  (testing "every VRM 1.0 preset keyword has a bridge entry"
    (is (= (set (map first vt/expression-preset-table))
           (set (keys bridge/preset->arkit-weights))))))

(deftest all-arkit-names-referenced-are-real-test
  (testing "every ARKit name in the bridge table is one generate-arkit-targets actually emits"
    (let [known (set blendshape/arkit-names)]
      (doseq [[preset weights] bridge/preset->arkit-weights]
        (doseq [[arkit-name _w] weights]
          (is (contains? known arkit-name)
              (str preset " references unknown ARKit target " arkit-name)))))))

(deftest expression-list-shape-test
  (testing "expression-list produces vrm-expression maps with resolved morph-index"
    (let [exprs (bridge/expression-list 3)
          happy (first (filter #(= :happy (:preset %)) exprs))]
      ;; vrm.vrm-types/expression-preset-table has 18 entries (the ADR said "17
      ;; presets", undercounting :neutral — verified against the real table via
      ;; covers-all-17-vrm-presets-test's set-equality check above).
      (is (= 18 (count exprs)))
      (is (= "happy" (:name happy)))
      (is (every? #(= 3 (:mesh-index %)) (:morph-target-binds happy)))
      (is (every? #(integer? (:morph-index %)) (:morph-target-binds happy)))
      (let [neutral (first (filter #(= :neutral (:preset %)) exprs))]
        (is (empty? (:morph-target-binds neutral)))))))
