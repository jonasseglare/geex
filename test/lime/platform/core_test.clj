(ns lime.codegen.core-test
  (:require [lime.codegen.core :refer :all]
            [clojure.test :refer :all]))

(deftest compile-static-value-test
  (is (= 9.0 (compile-static-value :clojure 9.0))))
