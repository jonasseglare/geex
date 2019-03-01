(ns geex.resolved-test
  (:require [geex.core :as core]
            [geex.java :as java]
            [geex.common :as lib]
            [clojure.test :refer :all]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  This file is for tests that previously did not work
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; This is the one that does not work.
(java/typed-defn find-index
                 [(lib/array-class Integer/TYPE) data
                  Integer/TYPE value]
                 (let [len (lib/count data)]
                   (core/Loop
                    [index 0]
                    (core/If
                     (lib/= index len)
                     -1
                     (let [x (lib/aget data index)]
                       (core/If (lib/= x value)
                                (lib/+ 1000 index)
                                (core/Recur (lib/inc index))))))))


(deftest nested-if-problem
  (doseq [[number-to-find expected] (map vector
                                         [0 1 2 3 4 5 6]
                                         [-1 -1 1000 1001 1002 -1 -1])]
    (is (= expected (find-index (int-array [2 3 4])
                                number-to-find)))))
