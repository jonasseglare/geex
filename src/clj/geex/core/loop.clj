(ns geex.core.loop
  (:require [clojure.spec.alpha :as spec]))

(spec/def ::fn-like (spec/or :fn fn?
                             :keyword keyword?))

(spec/def ::init any?)
(spec/def ::eval ::fn-like)
(spec/def ::loop? ::fn-like)
(spec/def ::result ::fn-like)
(spec/def ::next ::fn-like)

(spec/def ::args (spec/keys :req-un [::init   ;; Initial loop state
                                     ::eval   ;; Evaluates the loop state
                                     ::loop?  ;; Test if we should loop on the evaluated state
                                     ::result ;; Extract the loop result from the evaluated stae
                                     ::next   ;; Next loop state from the evaluated state
                                     ]))
