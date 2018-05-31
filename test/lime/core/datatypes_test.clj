(ns lime.core.datatypes-test
  (:require [lime.core.datatypes :refer :all]
            [clojure.test :refer :all]
            [clojure.set :as cljset]
            [bluebell.utils.core :as utils]))

#_(deftest subset-test
  (is (cljset/subset? (utils/keyset primitive-types)
                      (conj (utils/keyset sample-type-map)
                            java.lang.Void)))
  (is (primitive-type? java.lang.Double))
  (is (= (java-type-symbol-to-class 'java.lang.String<>)
         (class (make-array java.lang.String 0))))
  (is (= (java-type-symbol-to-class 'java.lang.String)
         java.lang.String))
  (is (= (java-type-symbol-to-class 'int)
         java.lang.Integer)))


