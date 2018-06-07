(ns geex.core.datatypes
  (:require [clojure.spec.alpha :as spec]
            [clojure.reflect :as r]
            [clojure.string :as cljstr])
  (:refer-clojure :exclude [void char boolean byte short int long float double]))

(defn add-sample-type [dst x]
  (assoc dst (class x) x))

(def sample-type-map (reduce add-sample-type
                             {}
                             [34.0
                              (clojure.core/float 3.4)
                              false
                              34
                              (bigint 34)
                              (bigdec 34.0)
                              3/4
                              (clojure.core/byte 3)
                              (clojure.core/short 3)
                              (clojure.core/int 3)
                              \a
                              :a]))

(defn array-class-of-type [tp]
  (class (make-array tp 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Java arrays
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn query-return-type [f args]
  (let [samples (map sample-type-map args)]
    (try
      (if (every? (complement nil?) samples)
        (class (apply f samples)))
      (catch Throwable e nil))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  For identifying things
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn boxed-type-symbol [name]
  (symbol (str "java.lang." name)))

(defn unboxed-type-symbol [name]
  (symbol (str "java.lang." name "/TYPE")))

(defn make-primitive-type-info [name]
  (eval {:unboxed-name name
         :unboxed-type (unboxed-type-symbol name)
         :boxed-type (boxed-type-symbol name)}))

(def unboxable-type-names
  ["Float"
   "Double"
   "Character"
   "Short"
   "Integer"
   "Long"
   "Boolean"
   "Void"])

(def primitive-type-list
  (map make-primitive-type-info
       unboxable-type-names))

(def boxed-to-unboxed-map (into {} (map (fn [p]
                                          [(:boxed-type p) (:unboxed-type p)])
                                        primitive-type-list)))

(defn unbox-class [x]
  (or (get boxed-to-unboxed-map x) x))

(defn unboxed-class-of [value]
  (-> value
      class
      unbox-class))

(defmacro inject-type-defs []
  `(do
     ~@(map (fn [info]
              `(def
                 ~(symbol (.getName (:unboxed-type info)))
                 ~(unboxed-type-symbol (:unboxed-name info))))
            primitive-type-list)))
