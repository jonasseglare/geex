(ns geex.java.try-block-test
  (:require [geex.java.try-block :refer :all]
            [clojure.test :refer :all]))

(deftest validation-test
  (is (validate {:body (fn [])}))
  (is (thrown? Exception (validate {})))
  (is (thrown? Exception (validate {:body [{:type 9}]})))
  (is (validate {:body identity
                 :catches [{:type Integer/TYPE
                            :body (fn [x] x)}]}))
  (is (validate {:body identity
                 :catches [{:type Integer/TYPE
                            :body (fn [x] x)}]
                 :finally identity}))
  (is (thrown?
       Exception
       (validate {:body identity
                  :catches [{:type Integer/TYPE
                             :body (fn [x] x)}]
                  :finally :kattskit}))))
