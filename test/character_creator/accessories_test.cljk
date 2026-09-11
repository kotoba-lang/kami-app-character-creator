(ns character-creator.accessories-test
  (:require [clojure.test :refer [deftest is testing]]
            [character.body :as cbody]
            [character-creator.accessories :as acc]))

(defn- bone-world-pos-by-name []
  (let [skeleton (cbody/generate-humanoid-skeleton)
        names (mapv :name (:bones skeleton))
        pos (cbody/bone-world-positions (:bones skeleton))]
    (zipmap names pos)))

(defn- sane-mesh?
  "No NaN/Inf, every index in bounds, indices count a multiple of 3."
  [{:keys [vertices indices]}]
  (and (seq vertices)
       (zero? (mod (count indices) 3))
       (every? #(<= 0 % (dec (count vertices))) indices)
       (every? (fn [{:keys [position normal]}]
                 (every? #(and (not (Double/isNaN %)) (not (Double/isInfinite %))) (concat position normal)))
               vertices)))

(deftest every-catalog-accessory-is-a-sane-mesh-test
  (testing "every accessory-catalog entry resolves to real, non-degenerate geometry"
    (let [bwp (bone-world-pos-by-name)]
      (doseq [id (keys acc/accessory-catalog)]
        (let [part (acc/generate-accessory-part id bwp)]
          (is (some? part) (str id " resolved to nil"))
          (is (sane-mesh? part) (str id " geometry is degenerate"))
          (is (= (name id) (:name part)))
          (is (= id (:material part))))))))

(deftest every-catalog-decal-is-a-sane-mesh-test
  (testing "every decal-catalog entry resolves to a real flat quad"
    (let [bwp (bone-world-pos-by-name)]
      (doseq [id (keys acc/decal-catalog)]
        (let [part (acc/generate-decal-part id bwp)]
          (is (some? part) (str id " resolved to nil"))
          (is (= 4 (count (:vertices part))))
          (is (= 6 (count (:indices part))))
          (is (sane-mesh? part) (str id " geometry is degenerate")))))))

(deftest unknown-ids-return-nil-test
  (testing "an id not in the catalog returns nil, not an error"
    (let [bwp (bone-world-pos-by-name)]
      (is (nil? (acc/generate-accessory-part :nope bwp)))
      (is (nil? (acc/generate-decal-part :nope bwp))))))

(deftest draw-pattern-matches-decal-origin-test
  (testing "draw-pattern's centre equals decal-origin (same coordinate space as the baked mesh)"
    (let [bwp (bone-world-pos-by-name)]
      (doseq [id (keys acc/decal-catalog)]
        (when-let [{:keys [params kind]} (acc/draw-pattern id bwp)]
          (let [origin (acc/decal-origin id bwp)
                [px py pz r] params]
            (is (= 2 kind))
            (is (= origin [px py pz]))
            (is (pos? r))))))))

(deftest draw-pattern-nil-for-no-pattern-entry-test
  (testing ":tattoo-shoulder has no :pattern in the catalog -> draw-pattern returns nil (flat colour, backward compatible)"
    (let [bwp (bone-world-pos-by-name)]
      (is (nil? (:pattern (:tattoo-shoulder acc/decal-catalog))))
      (is (nil? (acc/draw-pattern :tattoo-shoulder bwp))))))

(deftest accessories-are-positioned-near-their-attach-bone-test
  (testing "an accessory's vertices sit within a plausible radius of its attach bone (not at the origin/some default)"
    (let [bwp (bone-world-pos-by-name)
          part (acc/generate-accessory-part :cap-simple bwp)
          head-pos (get bwp "head")
          centroid (let [ps (map :position (:vertices part)) n (count ps)]
                     (mapv #(/ % n) (apply map + ps)))
          dist (Math/sqrt (apply + (map (fn [a b] (* (- a b) (- a b))) centroid head-pos)))]
      (is (< dist 0.3) "cap centroid should be close to the head bone, not far away"))))
