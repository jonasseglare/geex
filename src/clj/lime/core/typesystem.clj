(ns lime.core.typesystem
  (:require [bluebell.utils.setdispatch :as sd]
            [bluebell.utils.symset :as ss]
            [lime.core.seed :as seed]
            [bluebell.tag.core :as tg]
            [bluebell.utils.debug :as debug]
            [clojure.spec.alpha :as spec]
            [bluebell.utils.party :as party]))

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
(spec/def ::prefixed (spec/and vector?
                               (spec/cat :prefix keyword?
                                         :value any?)))

(spec/def ::suffixed (spec/and vector?
                               (spec/cat :value any?
                                         :suffix keyword?)
                               ))

(spec/def ::platform (spec/and vector?
                               (spec/cat :prefix #{:platform}
                                         :platform any?)
                               ))

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

(def platform-indicator (sd/spec-indicator ::platform (fn [x] #{[:platform (:platform x)]})))

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

(defn tagged-generator
  ([tag superset]
   (tagged-generator tag superset false))
  ([tag superset with-inner]
   (fn [set-reg x]
     (if (tg/tagged? tag x)
       (into #{superset} (if with-inner
                           (map (tg/tag tag)
                                (ss/direct-supersets-of
                                 set-reg
                                 (tg/value x)))
                           #{}))
       #{}))))

(def prefix-generator (tagged-generator :prefix :prefixed))
(def primitive-array-generator (tagged-generator :array :array))
(def suffix-generator (tagged-generator :suffix :suffixed))
(def typed-map-generator (tagged-generator :map-type :typed-map))
(def platform-generator (tagged-generator :platform :platform true))

(sd/register-superset-generator system seed-supersets)
(sd/register-superset-generator system class-supersets)
(sd/register-superset-generator system prefix-generator)
(sd/register-superset-generator system suffix-generator)
(sd/register-superset-generator system typed-map-generator)
(sd/register-superset-generator system primitive-array-generator)
(sd/register-superset-generator system platform-generator)

(defn primitive-array-class [element-class]
  (class (make-array element-class 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Static relations
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(sd/subset-of system :clojure :keyword)
(sd/subset-of system :java :keyword)
(sd/subset-of system :platform :any)

(sd/subset-of system :map :associative)
(sd/subset-of system :seq :sequential)
(sd/subset-of system :vector :sequential)
(sd/subset-of system :associative :coll)
(sd/subset-of system :sequential :coll)
(sd/subset-of system :coll :any)
(sd/subset-of system :prefixed :vector)
(sd/subset-of system :suffixed :vector)
(sd/subset-of system :typed-map :map)
(sd/subset-of system [:seed java.lang.Object] :any)
(sd/subset-of system :seed :any)
(sd/subset-of system java.lang.Object :class)
(sd/subset-of system :class :any)

(sd/subset-of system java.lang.Double :floating-point)
(sd/subset-of system java.lang.Float :floating-point)
(sd/subset-of system java.lang.Byte :integer)
(sd/subset-of system java.lang.Short :integer)
(sd/subset-of system java.lang.Integer :integer)
(sd/subset-of system java.lang.Long :integer)
(sd/subset-of system java.lang.Boolean :boolean)
(sd/subset-of system java.lang.Character :character)
(sd/subset-of system java.lang.Void :void)

(sd/subset-of system clojure.lang.BigInt :integer)
(sd/subset-of system :floating-point :real-number)
(sd/subset-of system :integer :real-number)
(sd/subset-of system clojure.lang.Ratio :real-number)
(sd/subset-of system :real-number :general-number)
(sd/subset-of system :general-number :any)


(sd/subset-of system :character :any)
(sd/subset-of system :boolean :any)
(sd/subset-of system :void :any)

(sd/subset-of system java.lang.Double/TYPE :floating-point :java-primitive)
(sd/subset-of system java.lang.Float/TYPE :floating-point :java-primitive)
(sd/subset-of system java.lang.Byte/TYPE :integer :java-primitive)
(sd/subset-of system java.lang.Short/TYPE :integer :java-primitive)
(sd/subset-of system java.lang.Integer/TYPE :integer :java-primitive)
(sd/subset-of system java.lang.Long/TYPE :integer :java-primitive)
(sd/subset-of system java.lang.Boolean/TYPE :boolean :java-primitive)
(sd/subset-of system java.lang.Character/TYPE :character :java-primitive)
(sd/subset-of system java.lang.Void/TYPE :void :java-primitive)
(sd/subset-of system :java-primitive :any)

(sd/subset-of system clojure.lang.Keyword :keyword)
(sd/subset-of system clojure.lang.Symbol :symbol)
(sd/subset-of system java.lang.String :string)

(sd/subset-of system :keyword :any)
(sd/subset-of system :symbol :any)
(sd/subset-of system :string :any)


(sd/subset-of system :array :java-array)
(sd/subset-of system (primitive-array-class Double/TYPE) [:array :floating-point])
(sd/subset-of system (primitive-array-class Float/TYPE) [:array :floating-point])
(sd/subset-of system (primitive-array-class Byte/TYPE) [:array :integer])
(sd/subset-of system (primitive-array-class Short/TYPE) [:array :integer])
(sd/subset-of system (primitive-array-class Integer/TYPE) [:array :integer])
(sd/subset-of system (primitive-array-class Long/TYPE) [:array :integer])
(sd/subset-of system (primitive-array-class Boolean/TYPE) [:array :any])
(sd/subset-of system (primitive-array-class Character/TYPE) [:array :any])

(sd/def-feature feature
  basic-indicator
  prefix-indicator
  suffix-indicator
  typed-map-indicator
  platform-indicator)
