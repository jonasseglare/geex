(ns lime.platform.core-test
  (:require [lime.platform.core :refer :all]
            [clojure.test :refer :all]))

(deftest compile-static-value-test
  (is (= 9.0 (compile-static-value 9.0))))
