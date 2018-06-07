(ns geex.core.datatypes
  (:require [clojure.spec.alpha :as spec]
            [clojure.reflect :as r]
            [clojure.string :as cljstr])
  (:refer-clojure :exclude [void char boolean byte short int long float double]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Declarations
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declare box-class)
(declare unbox-class)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Sample values
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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

(defn get-sample-value [x]
  (or (get sample-type-map x)
      (get sample-type-map (box-class x))))

(defn query-return-type [f args]
  (let [samples (map get-sample-value args)]
    (try
      (if (every? (complement nil?) samples)
        (unbox-class (class (apply f samples))))
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

(def unboxed-to-boxed-map (into {} (map (fn [p]
                                          [(:unboxed-type p) (:boxed-type p)])
                                        primitive-type-list)))

(defn unbox-class [x]
  (or (get boxed-to-unboxed-map x) x))

(defn box-class [x]
  (or (get unboxed-to-boxed-map x) x))

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

(def unboxed-type-set (into #{} (map :unboxed-type primitive-type-list)))

(defn unboxed-type? [x]
  {:pre [(class? x)]}
  (contains? unboxed-type-set x))

(def boxed-type? (complement unboxed-type?))
