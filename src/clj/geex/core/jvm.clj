(ns geex.core.jvm

  "Platform specific code needed by the compiler"
  
  (:require [bluebell.utils.ebmd :as ebmd]
            [bluebell.utils.ebmd.ops :as eops]
            [bluebell.utils.ebmd.type :as type]
            [geex.ebmd.type :as etype]
            [geex.core.seed :as seed]))


;;;------- Common type signatures for JVM platforms -------

(ebmd/declare-poly get-type-signature)

(ebmd/def-poly get-type-signature
  [(eops/and etype/seed-with-class
             etype/compilable-seed) x]
  (seed/datatype x))

(ebmd/def-poly get-type-signature
  [etype/class-arg x]
  x)

(ebmd/def-poly get-type-signature
  [type/map x]
  clojure.lang.IPersistentMap)

(ebmd/def-poly get-type-signature
  [type/set x]
  clojure.lang.IPersistentSet)

(ebmd/def-poly get-type-signature
  [type/any x]
  (if (vector? x)
    clojure.lang.IPersistentVector
    java.lang.Object))
