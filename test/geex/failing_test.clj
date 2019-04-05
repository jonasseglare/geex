(ns geex.failing-test
  (:require [geex.core :as core]
            [geex.core.seed :as seed]
            [geex.java :as java]
            [geex.common :as c]
            [clojure.test :refer :all]))

(def darr (c/array-type Double/TYPE))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Namespace dedicated for reproducing things that don't work.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


