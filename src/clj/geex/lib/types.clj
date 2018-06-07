(ns geex.lib.types
  (:require [geex.core.seed :as sd]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Implementation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro def-class-and-seed-from-proto [prefix-symbol proto-value]
  (let [prefix-str (name prefix-symbol)
        type-sym (symbol (str prefix-str "-type"))
        seed-sym (symbol (str  prefix-str "-seed"))]
    `(let [cl# (class ~proto-value)]
       (def ~type-sym cl#)
       (def ~seed-sym (sd/typed-seed cl#)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-class-and-seed-from-proto double (double 1.3))
(def-class-and-seed-from-proto float (float 1.3))
