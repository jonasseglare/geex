(ns geex.CodeMap-test
  (:import [geex CodeMap CodeItem])
  (:require [clojure.test :refer :all]))


(deftest codemap-test
  (let [cm (CodeMap.)]
    (.add cm (CodeItem. :a (fn [] 3) nil))
    (.add cm (CodeItem. :b (fn [] 4) nil))    
    (let [uc (.getUnorderedCode cm)
          c (vec uc)]
      (is (or (= c [3 4])
              (= c [4 3]))))
    (.mergeInPlace cm (doto (CodeMap.)
                        (.add (CodeItem. :c (fn [] 119) nil))))
    (is (= 3 (count (.getUnorderedCode cm))))))
