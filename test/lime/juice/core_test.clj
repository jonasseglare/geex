(ns lime.juice.core-test
  (:require [clojure.test :refer :all]
            [lime.juice.core :refer :all :as j]
            [lime.core :as lime])
  (:refer-clojure :exclude [+ - * /]))

(deftest dispatch-fn-test
  (is (= [:prefixed :kattskit] (dispatch-code [:kattskit 119])))
  (is (= [:suffixed :kattskit] (dispatch-code [119 :kattskit])))
  (is (= [:seed Double/TYPE] (dispatch-code (lime/to-seed 1.3))))
  (is (= [:typed-map :ad] (dispatch-code {:type :ad :value 119})))
  (is (= :number (dispatch-code 119))))

(deftest dispatch-code-vector-test
  (is (= (dispatch-code-vector [3.0 4])
         [[:seed java.lang.Double/TYPE] [:seed java.lang.Long/TYPE]]))
  (is (= (j/+ 1 2)
         3)))
