(ns lime.platform.low-test
  (:require [lime.platform.low :refer :all]
            [lime.core.seed :as seed]
            [clojure.reflect :as r]
            [clojure.test :refer :all]))

(deftest compile-static-value-test
  (is (= 9.0 (compile-static-value 9.0))))

(deftest type-signature-test
  (is (= "float" (r/typename java.lang.Float/TYPE)))
  (is (= "java.lang.String" (r/typename java.lang.String)))
  (is (= java.lang.String (get-type-signature [:platform :java]
                                                (seed/typed-seed java.lang.String))))
  (is (= java.lang.Float (get-type-signature [:platform :java]
                                     (seed/typed-seed java.lang.Float))))
  (is (= clojure.lang.IPersistentVector (get-type-signature [:platform :java]
                                                              [1 2 2]))))

#_(def a         #{[:seed :class] [:seed java.lang.Object]
                 [:seed java.io.Serializable] [:seed java.lang.Number]
                 :bluebell.utils.setdispatch/query-element
                 [:seed java.lang.Comparable] [:seed java.lang.Float]})

#_(def b         #{[:seed :java-primitive-number] [:seed java.lang.Number]
                 :bluebell.utils.setdispatch/query-element
                 [:seed :java-primitive] [:seed java.lang.Float]})
