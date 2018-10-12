(ns geex.java.class-test
  (:require [geex.java.class :refer :all :as jc]
            [geex.jcore :as core]
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
  (is (accumulator? (evaluate
                     (class-spec
                      Mjao
                      (private
                       (public (protected)))))))
  (let [k (evaluate
           (class-spec
            Mjao
            (extends java.lang.Integer)
            (extends java.lang.Double)))]
    (is (accumulator? k))
    (is (= [java.lang.Integer java.lang.Double] (:extends k))))
  (is (thrown?
       Exception
       (evaluate
        (class-spec
         Mjao
         (extends :a)))))
  (is (= [java.lang.String]
         (:implements (evaluate
                       (class-spec
                        Mu
                        (implements java.lang.String))))))
  (let [vars (:variables (evaluate
                          (class-spec
                           Macka
                           (static
                            (private
                             (variable [java.lang.Double] k))))))
        v (get vars "k")]

    (is (= {:name "k"
            :type [(core/typed-seed
                    java.lang.Double)]}
           (select-keys v
                        [:name :type])))
    (is (-> v
            :context
            :static?))
    (is (-> v
            :context
            :visibility
            (= :private)))))

(deftest basic-setter-getter-test
  (let [mummi (instantiate-object
               (class-spec
                Mummi 
                (public
                 (variable [java.lang.Double/TYPE
                            java.lang.Long] a
                           (setter setA)
                           (getter getA))
                 (method katt [Double/TYPE x]
                         [x x])
                 (method katt2 [Double/TYPE x]
                         {:a x})

                 ;; Not possible: Duplicate field name.
                 #_(method katt3 [Double/TYPE x]
                           {:a x})
                 )
                
                ))]
    (.setA mummi [0.3 4])
    (= [9.0 9.0] (.katt mummi 9.0))
    (= {:a 9.0} (.katt2 mummi 9.0))
    (is (= [0.3 4] (.getA mummi))))
  (let [mummi (instantiate-class
               (class-spec
                Mummi2 
                                        ;(extends java.lang.Integer)
                                        ;(implements java.lang.Double)

                (static
                 (public
                  (variable [java.lang.Double/TYPE
                             java.lang.Long] a
                            (setter setA)
                            (getter getA))))
                
                ))]
    (is (class? mummi))))


(deftest package-test
  (is (= "Katten" (:package (evaluate
                             (package
                              Katten
                              (class-spec Kattskit)))))))

(deftest path-test
  (is (= (.getPath
          (get-output-filename 
           {:output-prefix "src2/java"}
           (evaluate (package a.b (class-spec Katt)))))
         "src2/java/a/b/Katt.java"))
  (is (= (.getPath
          (get-output-filename 
           {:output-prefix "src2/java"}
           (evaluate (class-spec Katt))))
         "src2/java/Katt.java")))

(deftest double-static-code
  (let [obj (instantiate-object
             (class-spec Katts
                         (method mjao []
                                 {:a 3})
                         (method mu []
                                 {:a 4})))]
    (is (= (.mjao obj) {:a 3}))
    (is (= (.mu obj) {:a 4}))))

(deftest set-var-test
  (let [obj (instantiate-object
             (class-spec Katt2

                         (data-method getIt)
                         
                         (variable Double/TYPE a
                                   (getter getA))

                         (variable Integer/TYPE b)

                         (method getWrappedA []
                                 {:a (get-var "a")})

                         (method setTo119 [Integer/TYPE k])
                         ;(method setTo119 [Integer/TYPE k])
                         
                         (method setTo119 []
                                 (set-var
                                  "a" (core/wrap 119.0)))))]
    (is (= 0.0 (.getA obj)))
    (.setTo119 obj)
    (is (= 119.0 (.getA obj)))
    (is (= {:a 119.0
            :b 0} (.getIt obj)))
    (is (= {:a 119.0}
           (.getWrappedA obj)))))