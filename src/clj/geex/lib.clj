(ns geex.lib
  (:require [geex.core :as core]
            [clojure.core :as c]
            [geex.core.seed :as seed]
            [bluebell.utils.setdispatch :as setdispatch]
            [bluebell.utils.lufn :as lufn]
            [geex.core.typesystem :as ts]
            [geex.core.defs :as defs])
  (:refer-clojure :only [defn fn apply defmacro case comp]))

(defn seed-wrapper [predicate]
  (fn [x]
    (if (c/or (seed/seed? predicate)
              (c/not (predicate x)))
      x
      (core/to-seed x))))

(def number-to-seed (seed-wrapper c/number?))
(def char-to-seed (seed-wrapper c/char?))
(def string-to-seed (seed-wrapper c/string?))
(def keyword-to-seed (seed-wrapper c/keyword?))
(def symbol-to-seed (seed-wrapper c/symbol?))

(defn wrap-args [wrapper f]
  (fn [& args]
    (apply f (c/map wrapper args))))

(def wrap-numeric-args (c/partial wrap-args number-to-seed))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Various utilities
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn with-platform [f]
  (fn [& args]
    (apply f (c/conj (c/seq args) (defs/get-platform-tag)))))

(defn lufn-with-platform [f]
  (fn [& args]
    (apply f (c/conj (c/seq args) (defs/get-platform)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Polymorphic functions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Numeric operations
(def numeric-op (comp wrap-numeric-args with-platform))
(def numeric-lufn-op (comp wrap-numeric-args lufn-with-platform))

(def negate (numeric-op core/negate))
(def binary-add (numeric-op core/binary-add))
(def binary-sub (numeric-op core/binary-sub))
(def binary-div (numeric-op core/binary-div))
(def binary-mul (numeric-op core/binary-mul))

(defmacro generalize-binary-op [name
                                op
                                args
                                zero-arg-output
                                one-arg-output]
  `(defn ~name [& ~args]
     (c/case (c/count ~args)
       0 ~zero-arg-output
       1 ~one-arg-output
       (c/reduce ~op ~args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Outer API
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Forward decls
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def nil-of core/nil-of)
(def wrap core/to-seed)

;;;------- Common math operators -------

(generalize-binary-op + binary-add args
                      0
                      (c/first args))

(generalize-binary-op - binary-sub args
                      0
                      (negate (c/first args)))

(generalize-binary-op / binary-div args
                      (throw
                       (c/ex-info
                        "Insufficient number of arguments to /" {}))
                      (c/first args))

(generalize-binary-op * binary-mul args
                      1
                      (c/first args))




;;;------- Comparison operators -------

(def == (lufn-with-platform core/platform-==))
(def <= (lufn-with-platform core/platform-<=))
(def >= (lufn-with-platform core/platform->=))
(def > (lufn-with-platform core/platform->))
(def < (lufn-with-platform core/platform-<))
(def != (lufn-with-platform core/platform-!=))

(def = (lufn-with-platform core/platform-=))

;;;------- Logic operators -------

(defmacro and [& args]
  (if (c/empty? args)
    `(core/to-seed true)
    `(core/If ~(c/first args)
              (and ~@(c/rest args))
              (core/to-seed false))))

(defmacro or [& args]
  (if (c/empty? args)
    `(core/to-seed false)
    `(core/If ~(c/first args)
              (core/to-seed true)
              (or ~@(c/rest args)))))

(def not (with-platform core/platform-not))

(def not= (comp not =))

(defmacro implies [a b]
  `(or (not ~a) ~b))




;;;------- Array functions -------
