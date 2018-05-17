(ns lime.java.defs
  (:require [clojure.spec.alpha :as spec]))

(spec/def ::typed-argument (spec/cat :type any?
                                     :symbol symbol?))

(spec/def ::typed-arguments (spec/spec (spec/* ::typed-argument)))

(spec/def ::defn-args (spec/cat :name symbol?
                                :arglist ::typed-arguments
                                :body (spec/* any?)))
