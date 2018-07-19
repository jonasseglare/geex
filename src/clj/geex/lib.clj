(ns geex.lib
  (:require [geex.core :as core]
            [clojure.core :as c]
            [bluebell.utils.setdispatch :as setdispatch]
            [bluebell.utils.lufn :as lufn]
            [geex.core.typesystem :as ts]
            [geex.core.defs :as defs])
  (:refer-clojure :only [defn fn apply defmacro case]))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Various utilities
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn with-platform [f]
  (fn [& args]
    (apply f (c/conj (c/seq args) (defs/get-platform-tag)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Polymorphic functions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def binary-add (with-platform core/binary-add))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Outer API
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn +  [& args]
  (c/case (c/count args)
    0 0
    1 (c/first args)
    (c/reduce (c/completing binary-add) args)))

(defmacro my-and [& args]
  (c/println "---- mY AND on " args))

(defmacro and [& args]
  (c/println "---AND on" args)
  (if (c/empty? args)
    `(core/to-seed true)
    `(core/If ~(c/first args)
              (and ~@(c/rest args))
              ~(core/to-seed false))))
