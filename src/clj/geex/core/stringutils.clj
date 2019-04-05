(ns geex.core.stringutils
  (:require [clojure.string :as cljstr]))

;; Convention: Whenever risk of ambiguity,
;; a function should wrap its output in parenthesis.
;; But it is not its responsibility to wrap its input.
(defn wrap-in-parens [x]
  ["(" x ")"])

(defn join-spaced
  ([x] x)
  ([a b]
   (str a " " b)))

(defn nested-to-string
  [x]
  (cond
    (string? x) x
    (number? x) (str x)
    (vector? x) (transduce
                     (map nested-to-string)
                     join-spaced
                     ""
                     x)
    (seq? x)
    (throw (ex-info
            "Sequences are not supported, as they may be lazy"
            {:value x}))
    
    :default (throw (ex-info "Cannot convert to string"
                             {:value x}))))

(defn- kebab-from-camel-char [dst x]
  (if (Character/isUpperCase x)
    (str dst "-" (cljstr/lower-case x))
    (str dst x)))

(defn kebab-from-camel-case [src]
  (reduce
   kebab-from-camel-char
   ""
   src))
