(ns geex.java.class-test
  (:require [geex.java.class :refer :all :as jc]
            [bluebell.utils.dsl :as dsl]
            [clojure.spec.alpha :as spec]
            [clojure.test :refer :all]))

(deftest basic-test
  (let [class-spec (class-spec Kattskit   ;dsl/group
                    (extends java.lang.Double)
                    (implements java.lang.Integer)
                    (static (method mummi []))
                    (private (variable Double/TYPE kattskit))
                    (variable Double/TYPE mjao)
                    (implements java.lang.String))
        result (class-spec
                empty-class-def)
        methods (:methods result)
        m (first methods)]
    (is (= [java.lang.Integer java.lang.String]
           (:implements result)))
    (is (= [java.lang.Double]
           (:extends result)))
    (is (= 'Kattskit (:name result)))
    (is (= 1 (count methods)))
    (is (contains? m :settings))
    (is (:static? (:settings m)))
    (is (= :public (:visibility (:settings m))))
    (let [v (first (:variables result))]
      (is (= :private (-> v :settings :visibility))))))

