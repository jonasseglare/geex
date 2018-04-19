(ns lime.core.loop
  (:require [clojure.spec.alpha :as spec]))

(spec/def ::init any?)
(spec/def ::eval fn?)
(spec/def ::loop? fn?)
(spec/def ::result fn?)
(spec/def ::next fn?)

(spec/def ::args (spec/keys :req-un [::init   ;; Initial loop state
                                     ::eval   ;; Evaluates the loop state
                                     ::loop?  ;; Test if we should loop on the evaluated state
                                     ::result ;; Extract the loop result from the evaluated stae
                                     ::next   ;; Next loop state from the evaluated state
                                     ]))
