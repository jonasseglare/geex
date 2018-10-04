(ns examples.circle-fit-test
  (:require [geex.lib :as lib]
            [geex.core :as core]
            [geex.java :as java]
            [clojure.spec.alpha :as spec]
            [clojure.test :refer :all]))

(spec/def ::derivatives (spec/coll-of any?))
(spec/def ::value any?)
(spec/def ::ad (spec/keys :req-un [::value ::derivatives]))

(def ad? (partial spec/valid? ::ad))

(defn add [x y]
  {:pre [(ad? x)
         (ad? y)]
   :post [(ad? %)]}
  {:value (lib/+ (:value x) (:value y))
   :derivatives (map lib/+
                     (:derivatives x)
                     (:derivatives y))})

(defn mul [x y]
  {:pre [(ad? x)
         (ad? y)]
   :post [(ad? %)]}
  {:value (lib/* (:value x) (:value y))
   :derivatives (mapv (fn [dx dy]
                        (lib/+ (lib/* (:value x) dy)
                               (lib/* (:value y) dx)))
                      (:derivatives x)
                      (:derivatives y))})

(defn sqr [x]
  (mul x x))

(defn negate [x]
  {:value (lib/negate (:value x))
   :derivatives (mapv lib/negate (:derivatives x))})

(defn sub [x y]
  (add x (negate y)))

(defn sqrt [x]
  (let [value (lib/sqrt (:value x))]
    {:value value
     :derivatives (mapv (fn [d] (lib/* d (lib// 0.5 value)))
                        (:derivatives x))}))


(deftest various-ad-tests
  (is (= (java/eval
          (add {:value 3.0
                :derivatives [2 3]}
               {:value 4.0
                :derivatives [5 8]}))
         {:value 7.0 :derivatives [7 11]}))
  (is (= (java/eval
          (mul {:value 3.0
                :derivatives [2 3]}
               {:value 4.0
                :derivatives [5 8]}))
         {:value 12.0 :derivatives [23.0 36.0]})))



