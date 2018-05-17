(ns lime.platform.low-test
  (:require [lime.platform.low :refer :all]
            [clojure.test :refer :all]))

(deftest compile-static-value-test
  (is (= 9.0 (compile-static-value 9.0))))
