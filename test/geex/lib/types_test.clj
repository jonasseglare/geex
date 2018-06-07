(ns geex.lib.types-test
  (:require [geex.lib.types :refer :all]
            [clojure.test :refer :all]))

(deftest basic-types
  (is (= java.lang.Double double-type)))
