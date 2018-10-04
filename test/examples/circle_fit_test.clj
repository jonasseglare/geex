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
                        (lib/+ (lib/* x dy)
                               (lib/* y dx)))
                      (:derivatives x)
                      (:derivatives y))})

(deftest various-ad-tests
  (is (= (java/eval
          (add {:value 3.0
                :derivatives [2 3]}
               {:value 4.0
                :derivatives [5 8]}))
         {:value 7.0 :derivatives [7 11]}))
  #_(is (= (java/eval
          (mul {:value 3.0
                :derivatives [2 3]}
               {:value 4.0
                :derivatives [5 8]}))
         {:value 12.0 :derivatives [23.0 36.0]})))



