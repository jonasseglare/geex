(ns examples.matrix-test
  (:require [clojure.spec.alpha :as spec]
            [clojure.test :refer :all]
            [geex.lib :as l]
            [geex.java :as java]
            [geex.core :as core]))

(spec/def ::rows any?)
(spec/def ::cols any?)
(spec/def ::data any?)
(spec/def ::matrix (spec/keys :req-un [::rows ::cols ::data]))

(def matrix? (partial spec/valid? ::matrix))

(defn get-element [matrix i j]
  {:pre [(matrix? matrix)]}
  (l/aget (:data matrix) (l/cast Integer/TYPE (l/+ i (l/* j (:rows matrix))))))



;; Just to check that get-element works
(java/typed-defn
 get-element-f
 [{:rows Long/TYPE
   :cols Long/TYPE
   :data (l/array-class Double/TYPE)} matrix
  Long/TYPE i
  Long/TYPE j]
 (get-element matrix i j))

(deftest various-tests
  (is (= (get-element-f
          {:rows 3
           :cols 2
           :data
           (double-array (range 6))}
          1 1)
         4.0)))

