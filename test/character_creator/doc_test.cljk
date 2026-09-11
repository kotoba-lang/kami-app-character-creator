(ns character-creator.doc-test
  (:require [clojure.test :refer [deftest is testing]]
            [character-creator.doc :as doc]))

(deftest default-character-doc-test
  (testing "produces a well-formed CharacterDoc"
    (let [d (doc/default-character-doc)]
      (is (string? (:character/id d)))
      (is (map? (:character/def d)))
      (is (= [0.94 0.87 0.82] (get-in d [:character/def :skin :tone]))))))

(deftest boot-config-applies-palette-test
  (testing "palette merges into the nested CharacterDef fields, not a parallel slot"
    (let [d (doc/boot-config {:palette {:skin [0.1 0.2 0.3] :hair [0.4 0.5 0.6] :eye [0.7 0.8 0.9]}})]
      (is (= [0.1 0.2 0.3] (get-in d [:character/def :skin :tone])))
      (is (= [0.4 0.5 0.6] (get-in d [:character/def :hair :color])))
      (is (= [0.7 0.8 0.9] (get-in d [:character/def :eyes :iris-color]))))))

(deftest boot-config-applies-presets-and-overrides-test
  (testing "hair/clothing presets and arbitrary path overrides land on the def"
    (let [d (doc/boot-config {:hair-preset :buzz
                               :clothing-preset :hoodie
                               :overrides [[[:body :height] 1.1]]})]
      (is (= :buzz (get-in d [:character/def :hair :preset])))
      (is (= :hoodie (get-in d [:character/def :clothing :preset])))
      (is (= 1.1 (get-in d [:character/def :body :height]))))))
