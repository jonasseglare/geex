(ns lime.juice.core
  (:require [lime.core :as lime]
            [clojure.spec.alpha :as spec])
  (:refer-clojure :exclude [+ - * /]))

(spec/def ::prefixed (spec/cat :prefix keyword?
                               :value any?))

(spec/def ::suffixed (spec/cat :value any?
                               :suffix keyword?))

(spec/def ::type any?)
(spec/def ::typed-map (spec/keys :req-un [::type]))

(spec/def ::value (spec/or :prefixed ::prefixed
                           :suffixed ::suffixed
                           :seed ::lime/basic-seed
                           :typed-map ::typed-map
                           :vector vector?
                           :set set?
                           :seq seq?
                           :map map?
                           :keyword keyword?
                           :symbol symbol?
                           :number number?))
