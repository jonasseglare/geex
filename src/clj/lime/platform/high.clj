(ns lime.platform.high

  "High-level platform specific code that is not needed by the code generator. Therefore, this code can depend on the core module."

  (:require [lime.core :as lime]
            [lime.platform.low :as platform]
            [lime.core.defs :as defs]
            [lime.core.seed :as sd]
            [bluebell.utils.core :as utils]
            [bluebell.utils.defmultiple :refer [defmultiple]]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Casting to a type
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn flatten-str
  "Recursively concatenate all substrings into a single string, without indentation or line breaks."
  [x]
  (utils/indent-nested {:prefix "" :step ""} x))

(defn flatten-nested
  "Flatten the nested vectors into a nested multiline string"
  [x]
  (utils/indent-nested x))


(defn wrap-in-parens [x]
  (flatten-str ["(" x ")"]))

(defmultiple compile-cast defs/platform-dispatch
  (defs/java-platform
    [comp-state expr cb]
    (cb (defs/compilation-result
          comp-state
          (wrap-in-parens
           ["(" (.getName (sd/datatype expr)) ")"
            (-> expr
                defs/access-compiled-deps
                :value)])))))

(defn cast-seed [type value]
  (lime/with-new-seed
    "cast-seed"
    (fn [seed]
      (-> seed
          (sd/add-deps {:value value})
          (sd/compiler compile-cast)
          (sd/datatype type)))))

(defn retag-datatype [type seed]
  (lime/indirect seed #(sd/datatype % type)))

(defmultiple cast-to-type-sub defs/platform-dispatch
  (defs/clojure-platform [type value] (retag-datatype type value)) 
  (defs/java-platform [type value] (cast-seed type value)))

(defn cast-to-type
  "Casts to a type, if needed. In Clojure, no need to cast because it is dynamic. In java, we explicitly cast."
  [type value0]
  (let [value-seed (lime/to-seed value0)]
    (if (= (sd/datatype value-seed) type)
      value-seed
      (cast-to-type-sub type value-seed))))
