(ns geex.platform.high

  "High-level platform specific code that is not needed by the code generator. Therefore, this code can depend on the core module."

  (:require [geex.core :as geex]
            [geex.platform.low :as platform]
            [geex.core.defs :as defs]
            [geex.core.datatypes :as dt]
            [geex.core.seed :as sd]
            [geex.core.stringutils :as su :refer [wrap-in-parens]]
            [bluebell.utils.core :as utils]
            [bluebell.utils.defmultiple :refer [defmultiple]]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Implementation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (geex/with-new-seed
    "cast-seed"
    (fn [seed]
      (-> seed
          (sd/add-deps {:value value})
          (sd/compiler compile-cast)
          (sd/datatype type)))))

(defn retag-datatype [type seed]
  (geex/indirect seed #(sd/datatype % type)))

(defmultiple cast-to-type-sub defs/platform-dispatch
  (defs/clojure-platform [type value] (retag-datatype type value)) 
  (defs/java-platform [type value] (cast-seed type value)))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Code generation utilities
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Casting to a type
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn cast-to-type
  "Casts to a type, if needed. In Clojure, no need to cast because it is dynamic. In java, we explicitly cast."
  [type value0]
  (let [value-seed (geex/to-seed value0)]
    (if (= (sd/datatype value-seed) type)
      value-seed
      (cast-to-type-sub type value-seed))))
