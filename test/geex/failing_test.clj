(ns geex.failing-test
  (:require [geex.core :as core]
            [geex.core.seed :as seed]
            [geex.java :as java]
            [geex.common :as c]
            [clojure.test :refer :all]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Namespace dedicated for reproducing things that don't work.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#_(java/typed-defn sum-of-odd [(c/array-type Double/TYPE) x]
                 (c/reduce
                  c/+
                  0.0
                  (c/filter c/odd? x)))



