(ns lime.core.typesystem
  (:require [bluebell.utils.setdispatch :as sd]
            [bluebell.utils.symset :as ss]
            [lime.core.seed :as seed]
            [bluebell.tag.core :as tg]
            [clojure.spec.alpha :as spec]))

(sd/def-system system)

(defn tag-as-seed [x]
  [:seed x])

(defn tagged-as-seed? [x]
  (tg/tagged? :seed x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Specs
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(spec/def ::prefixed (spec/cat :prefix keyword?
                               :value any?))

(spec/def ::suffixed (spec/cat :value any?
                               :suffix keyword?))

(spec/def ::type any?)
(spec/def ::typed-map (spec/keys :req-un [::type]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Indicators
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn basic-indicator [x]
  (cond
    (seed/seed? x) #{(tag-as-seed (seed/datatype x))}
    (map? x) #{:map}
    (set? x) #{:set}
    (vector? x) #{:vector}
    (seq? x) #{:seq}
    :default #{(class x)}))

(def prefix-indicator (sd/spec-indicator ::prefixed (fn [x] #{[:prefix (:prefix x)]})))
(def suffix-indicator (sd/spec-indicator ::suffixed (fn [x] #{[:suffix (:suffix x)]})))
(def typed-map-indicator (sd/spec-indicator ::typed-map (fn [x] #{ [:map-type (:type x)]})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Superset generators
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn seed-supersets [set-reg x]
  (if (tagged-as-seed? x)
    (let [supersets (ss/direct-supersets-of set-reg (tg/value x))]
      (conj (set (map tag-as-seed supersets))
            :seed))))

(defn class-supersets [set-reg x]
  (if (class? x)
    (conj (set (supers x))
          :class)))

(defn tagged-generator [tag superset]
  (fn [set-reg x]
    (if (tg/tagged? tag x)
      #{superset}
      #{})))

(def prefix-generator (tagged-generator :prefix :prefixed))
(def suffix-generator (tagged-generator :suffix :suffixed))
(def typed-map-generator (tagged-generator :map-type :typed-map))

(sd/register-superset-generator system seed-supersets)
(sd/register-superset-generator system class-supersets)
(sd/register-superset-generator system prefix-generator)
(sd/register-superset-generator system suffix-generator)
(sd/register-superset-generator system typed-map-generator)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Static relations
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(sd/subset-of system :map :associative)
(sd/subset-of system :seq :sequential)
(sd/subset-of system :vector :sequential)
(sd/subset-of system :associative :coll)
(sd/subset-of system :sequential :coll)
(sd/subset-of system :coll :any)
(sd/subset-of system :prefixed :vector)
(sd/subset-of system :suffixed :vector)
(sd/subset-of system :typed-map :map)
(sd/subset-of system java.lang.Object :any)
(sd/subset-of system [:seed java.lang.Object] :any)
(sd/subset-of system :seed :any)
(sd/subset-of system :class :any)


(sd/def-feature feature
  basic-indicator
  prefix-indicator
  suffix-indicator
  typed-map-indicator)


