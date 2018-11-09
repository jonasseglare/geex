(ns geex.core.jvm

  "Platform specific code needed by the compiler"
  
  (:require [bluebell.utils.ebmd :as ebmd]
            [bluebell.utils.ebmd.ops :as eops]
            [bluebell.utils.ebmd.type :as type]
            [geex.ebmd.type :as etype]
            [geex.core.seed :as seed]))


;;;------- Common type signatures for JVM platforms -------

(ebmd/declare-poly get-type-signature)

(ebmd/def-arg-spec nil-arg {:pred nil?
                            :pos [nil]
                            :neg [:a]})

(ebmd/def-poly get-type-signature
  [etype/seed-with-class x]
  (seed/datatype x))

(ebmd/def-poly get-type-signature
  [etype/nothing-seed x]
  Void/TYPE)

(ebmd/def-poly get-type-signature
  [nil-arg x]
  Void/TYPE)

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


;; Get a type signature that can be compiled
(ebmd/declare-poly get-compilable-type-signature)

(ebmd/def-poly get-compilable-type-signature
  [type/any x]
  (get-type-signature x))

(ebmd/def-poly get-compilable-type-signature
  [(eops/and etype/seed-with-class
             (eops/not etype/compilable-seed)) x]
  clojure.lang.IPersistentMap)

