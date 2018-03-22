(ns lime.juice.core
  (:require [lime.core :as lime]
            [bluebell.utils.core :as utils]
            [bluebell.utils.specutils :as su]
            [bluebell.utils.defmultiple :refer [defmultiple]]
            [clojure.spec.alpha :as spec]
            [bluebell.utils.debug :as debug]
            [lime.impl.samplevalues :as samplevalues]
            [bluebell.tag.core :as tag])
  (:refer-clojure :exclude [+ - * /]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Specs
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Implementation details
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn dispatch-code
  "Function used to identify the type of value that we are dealing with."
  [value]
  (let [[value-type parsed-value] (su/force-conform ::value value)]
    (case value-type
      :prefixed [:prefixed (:prefix parsed-value)]
      :suffixed [:suffixed (:suffix parsed-value)]
      :seed [:seed (lime/datatype parsed-value)]
      :typed-map [:typed-map (:type parsed-value)]
      value-type)))



(def raw-types-to-normalize #{:symbol :number :keyword})

(defn normalize-value [x]
  (let [c (dispatch-code x)]
    (if (contains? raw-types-to-normalize c)
      (lime/to-seed x)
      x)))

(def normalized-dispatch-code (comp dispatch-code normalize-value))


(defn dispatch-code-vector [args]
  (mapv normalized-dispatch-code args))

(defn try-result [op]
  (fn [v]
    (println "op=" op)
    (println "v=" v)
    (or (and (every? (tag/tagged? :seed) v)
             (if-let [c (samplevalues/query-return-type op (map tag/value v))]
               :result))
        v)))

(defn dispatch-code-vector-or-numeric-result [op]
  (comp (try-result op)
        dispatch-code-vector))

(def basic-add-dispatch (dispatch-code-vector-or-numeric-result clojure.core/+))


(defn basic-add-seed [args]
  (-> (lime/initialize-seed "basic-add-seed")
      ))

(defmultiple basic-add basic-add-dispatch
  (:result [& args] (basic-add-seed args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Public functions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn +
  ([& args]
   (basic-add args)))
