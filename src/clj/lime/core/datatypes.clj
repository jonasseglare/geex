(ns lime.core.datatypes
  (:require [clojure.spec.alpha :as spec]
            [clojure.reflect :as r]
            [clojure.string :as cljstr]))

(defn add-sample-type [dst x]
  (assoc dst (class x) x))

(def sample-type-map (reduce add-sample-type
                             {}
                             [34.0
                              (float 3.4)
                              false
                              34
                              (bigint 34)
                              (bigdec 34.0)
                              3/4
                              (byte 3)
                              (short 3)
                              (int 3)
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


(defn make-boxed-unboxed-pair [name]
  (eval [(symbol (str "java.lang." name))
         (symbol (str "java.lang." name "/TYPE"))]))

(def boxed-to-unboxed-map (into {} (map make-boxed-unboxed-pair
                                        ["Float"
                                         "Double"
                                         "Character"
                                         "Short"
                                         "Integer"
                                         "Long"
                                         "Boolean"
                                         "Void"])))

(defn unbox-class [x]
  (or (get boxed-to-unboxed-map x) x))

(defn unboxed-class-of [value]
  (-> value
      class
      unbox-class))
