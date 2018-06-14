(ns geex.core.seedtype
  (:require [geex.core.seed :as sd]
            [geex.core.datatypes :as dt])
  (:refer-clojure :exclude [boolean char void byte short int long float double]))

(defmacro inject-seed-defs []
  `(do
     ~@(map (fn [info]
              `(def ~(symbol (str (.getName (:unboxed-type info))))
                 (sd/typed-seed ~(dt/unboxed-type-symbol (:unboxed-name info)))))
            dt/primitive-type-list)))

(inject-seed-defs)

(defmacro def-seed-type [name-sym class-sym]
  `(def ~name-sym (sd/typed-seed ~class-sym)))

(def-seed-type string java.lang.String)
