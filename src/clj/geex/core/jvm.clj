(ns geex.core.jvm

  "Platform specific code needed by the compiler"
  
  (:require [bluebell.utils.wip.setdispatch :as sd]
            [geex.core.seed :as seed]
            [geex.core.typesystem :as ts]
            ))


;;;------- Common type signatures for JVM platforms -------

(sd/def-dispatch get-type-signature ts/system ts/feature)

(sd/def-set-method get-type-signature
  "A seed with a general Java class"
  [[[:seed :class] x]]
  (seed/datatype x))

(sd/def-set-method get-type-signature
  "A Java class, not a primitive"
  [[:class x]]
  x)


(sd/def-set-method get-type-signature
  "A vector"
  [[:vector x]]
  clojure.lang.IPersistentVector)

(sd/def-set-method get-type-signature
  "A map"
  [[:map x]]
  clojure.lang.IPersistentMap)

(sd/def-set-method get-type-signature
  "A set"
  [[:set x]]
  clojure.lang.IPersistentSet)

(sd/def-set-method get-type-signature
  "Anything else"
  [[:any x]]
  java.lang.Object)
