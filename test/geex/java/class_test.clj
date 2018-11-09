(ns geex.java.class-test
  (:require [geex.java.class :refer :all :as jc]
            [geex.core :as core]
            [bluebell.utils.dsl :as dsl]
            [clojure.spec.alpha :as spec]
            [clojure.test :refer :all]))


(deftest class-def-test
  (is (class-def? {:name "Kattskit"}))
  (is (not (class-def? {:name 119})))
  (is (class-def?
       {:name "Mjao"
        :methods
        [{:name "Kattskit"
          :arg-types [Integer/TYPE]
          :fn (fn [x]
                (+ x 1))}]}))
  (is (not
       (class-def?
        {:name "Mjao"
         :methods
         [{:name "Kattskit"
           :arg-types [Integer/TYPE]
           :fn :adsf}]})))


  (is (valid?
       (validate-class-def
        {:name "Mjao"
         :methods [{:name "Mjao"
                    :arg-types [Integer/TYPE]
                    :fn identity}]})))
  (is (thrown? Exception
               (validate-class-def
                {:name "Mjao"
                 :methods [{:name "Mjao"
                            :arg-types [Integer/TYPE]
                            :fn identity}
                           {:name "Mjao"
                            :arg-types [Integer/TYPE]
                            :fn +}]})))
  (let [cd0 {:name "Mjao"
             :methods [{:name "a"
                        :arg-types [Double/TYPE]}]}
        cd (validate-class-def
            cd0)]
    (is (abstract? cd0))
    (is (abstract? cd))
    (is (not (interface? cd)))
    (is (valid? cd)))
  (let [cd0 {:name "Mjao"
             :methods [{:name "a"
                        :arg-types [Double/TYPE]
                        :fn (fn [x] x)}]}
        cd (validate-class-def
            cd0)]
    (is (not (abstract? cd0)))
    (is (not (abstract? cd)))
    (is (not (interface? cd)))
    (is (valid? cd)))
  (let [cd0 {:name "Mjao"
             :interface? true
             :methods [{:name "a"
                        :arg-types [Double/TYPE]
                        :ret Double/TYPE}]}
        cd (validate-class-def
            cd0)]
    
    (is (abstract? cd0))
    (is (abstract? cd))
    (is (interface? cd))
    (is (valid? cd)))
  (let [cd0 {:name "Mjao"
             :interface? true
             :methods [{:name "a"
                        :static? true
                        :arg-types [Double/TYPE]
                        :ret Double/TYPE}]}]
    (is (thrown? Exception (validate-class-def cd0))))
  (let [cd0 {:name "Mjao"
             :methods [{:name "a"
                        :arg-types [Double/TYPE]
                        :ret Double/TYPE}]}
        cd (validate-class-def cd0)]
    
    (is (abstract? cd0))
    (is (abstract? cd))
    (is (not (interface? cd)))
    (is (valid? cd))))
