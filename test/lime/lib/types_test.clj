(ns lime.lib.types-test
  (:require [lime.lib.types :refer :all]
            [clojure.test :refer :all]))

(deftest basic-types
  (is (= java.lang.Double double-type)))
