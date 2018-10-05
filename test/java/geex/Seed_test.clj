(ns geex.Seed-test
  (:import [geex Seed DynamicSeed])
  (:require [clojure.test :refer :all]))

(deftest seed-test
  (let [x (DynamicSeed.)]
    (is x)))
