(ns geex.core.datatypes-test
  (:require [geex.core.datatypes :refer :all]
            [clojure.test :refer :all]
            [clojure.set :as cljset]
            [bluebell.utils.core :as utils]))

(deftest unboxing-test
  (is (= java.lang.Long/TYPE
         (unboxed-class-of 9))))


