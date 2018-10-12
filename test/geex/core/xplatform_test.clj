(ns geex.core.xplatform-test
  (:require [geex.core.xplatform :refer :all]
            [geex.core.defs :as defs]
            [clojure.test :refer :all])
  (:refer-clojure :exclude [get]))


(deftest basic-test
  (register :kattskit {:exp (fn [r] [:exponent r])})
  (register :kattskit {:log (fn [x] [:logarithm x])
                       :pi 3.14159})
  
  (binding [defs/the-platform :kattskit]
    (is (= 3.14159 (get :pi)))
    (is (= [:logarithm 3] (call :log 3)))
    (is (= [:exponent 4] (call :exp 4))))
  (binding [defs/the-platform :mjao]
    (is (thrown? Exception (get :pi))))
  (is (contains? (set (list-platforms)) :kattskit))
  (swap! platform-map #(dissoc % :kattskit))
  (is (not (contains? (set (list-platforms)) :kattskit))))
