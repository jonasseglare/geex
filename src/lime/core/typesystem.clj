(ns lime.core.typesystem
  (:require [bluebell.utils.setdispatch :as sd]
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Superset generators
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn seed-supersets [x]
  (if (tagged-as-seed? x)
    (set (map tag-as-seed (supers (tg/value x))))))

(defn class-supersets [x]
  (if (class? x)
    (set (supers x))))

(defn prefix-suffix-generators [x]
  (if (or (tg/tagged? :prefix x)
          (tg/tagged? :suffix x))
    #{:vector}))

(sd/register-superset-generator system seed-supersets)
(sd/register-superset-generator system prefix-suffix-generators)

(sd/def-feature feature
  basic-indicator
  prefix-indicator
  suffix-indicator)


