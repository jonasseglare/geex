(ns lime.lib.types
  (:require [lime.core.seed :as sd]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Implementation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro def-class-and-seed-from-proto [prefix-symbol proto-value]
  (let [prefix-str (name prefix-symbol)
        type-sym (symbol (str prefix-str "-type"))
        seed-sym (symbol (str  prefix-str "-seed"))]
    (println "Expanding it" prefix-str)
    `(do
       (def ~type-sym ~(class proto-value))
       (def ~seed-sym (sd/typed-seed ~(class proto-value)))
       )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-class-and-seed-from-proto double (double 1.3))
(def-class-and-seed-from-proto float (float 1.3))
