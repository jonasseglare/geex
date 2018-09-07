(ns geex.java.class-test
  (:require [geex.java.class :refer :all :as jc]
            [bluebell.utils.dsl :as dsl]
            [clojure.spec.alpha :as spec]
            [clojure.test :refer :all]))

#_(deftest basic-test
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


(deftest basic-test
  (is (accumulator? (class-spec
                     Mjao
                     (private
                      (public (protected))))))
  (let [k (class-spec
           Mjao
           (extends java.lang.Integer)
           (extends java.lang.Double))]
    (is (accumulator? k))
    (is (= [java.lang.Integer java.lang.Double] (:extends k))))
  (is (thrown?
       Exception
       (class-spec
        Mjao
        (extends :a))))
  (is (= [java.lang.String]
         (:implements (class-spec
                       Mu
                       (implements java.lang.String)))))
  (let [vars (:variables (class-spec
                          Macka
                          (static
                           (private
                            (variable [java.lang.Double] k)))))
        v (get vars "k")]

    (is (= {:name "k"
            :type [java.lang.Double]}
           (select-keys v
                        [:name :type])))
    (is (-> v
            :context
            :static?))
    (is (-> v
            :context
            :visibility
            (= :private)))))
