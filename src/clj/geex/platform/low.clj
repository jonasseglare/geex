(ns geex.platform.low

  "Platform specific code needed by the compiler"
  
  (:require [bluebell.utils.defmultiple :refer [defmultiple defmultiple-extra]]
            [bluebell.utils.core :as utils]
            [geex.core.defs :as defs]
            [bluebell.utils.setdispatch :as sd]
            [geex.core.seed :as seed]
            [clojure.reflect :as r]
            [geex.core.typesystem :as ts]
            [bluebell.utils.symset :as ss]
            [geex.core.stringutils :as su]
            [clojure.string :as cljstr]
            ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Code generators
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(sd/def-dispatch get-type-signature ts/system ts/feature)

(sd/def-set-method get-type-signature
  "A seed with a general Java class"
  [[:platform p]
   [[:seed :class] x]]
  (seed/datatype x))

(sd/def-set-method get-type-signature
  "A Java class, not a primitive"
  [[:platform p]
   [:class x]]
  x)


(sd/def-set-method get-type-signature
  "A vector"
  [[:platform p]
   [:vector x]]
  clojure.lang.IPersistentVector)

(sd/def-set-method get-type-signature
  "A map"
  [[:platform p]
   [:map x]]
  clojure.lang.IPersistentMap)

(sd/def-set-method get-type-signature
  "A set"
  [[:platform p]
   [:set x]]
  clojure.lang.IPersistentSet)

(sd/def-set-method get-type-signature
  "Anything else"
  [[:platform p]
   [:any x]]
  java.lang.Object)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Identifiers on Java
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn str-to-java-identifier [& args]
  (-> (cljstr/join "_" args)
      (cljstr/replace "_" "__")
      (cljstr/replace "-" "_d")
      (cljstr/replace ":" "_c")
      (cljstr/replace "/" "_s")
      (cljstr/replace "." "_p")
      (cljstr/replace "?" "_q")))


(sd/def-dispatch to-java-identifier ts/system ts/feature)

(sd/def-set-method to-java-identifier [[:symbol x]]
  (str-to-java-identifier (name x)))

(sd/def-set-method to-java-identifier [[:string x]]
  (str-to-java-identifier x))
