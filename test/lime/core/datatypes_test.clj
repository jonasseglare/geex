(ns lime.core.datatypes-test
  (:require [lime.core.datatypes :refer :all]
            [clojure.test :refer :all]
            [clojure.set :as cljset]
            [bluebell.utils.core :as utils]))

(deftest unboxing-test
  (is (= java.lang.Long/TYPE
         (unboxed-class-of 9))))


