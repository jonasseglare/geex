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


  (is (= :success
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
                            :fn +}]}))))
