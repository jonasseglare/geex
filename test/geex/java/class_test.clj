(ns geex.java.class-test
  (:require [geex.java.class :refer :all :as jc]
            [clojure.spec.alpha :as spec]
            [clojure.test :refer :all]))

(deftest spec-test
  (is (spec/valid? ::jc/class-def '(ClassName)))
  (is (not (spec/valid? ::jc/class-def '(ClassName :kattskit))))
  (is (spec/valid? ::jc/class-def '(Class [:private])))
  (is (spec/valid? ::jc/class-def '(Class [:private] [:public])))
  (is (spec/valid? ::jc/classes '[a b c]))
  (is (spec/valid? ::jc/class-def '(ClassName :implements [a b c]
                                              ;:extends [x y z]
                                              ))))
