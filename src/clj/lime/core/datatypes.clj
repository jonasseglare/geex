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

(defn arrc [constructor]
  (class (constructor [])))

#_(def primitive-types {java.lang.Float {:java-name "float"
                                       :array-type (arrc float-array)}
                      java.lang.Double {:java-name "double"
                                        :array-type (arrc double-array)}
                      java.lang.Integer {:java-name "int"
                                         :array-type (arrc int-array)}
                      java.lang.Long {:java-name "long"
                                      :array-type (arrc long-array)}
                      java.lang.Byte {:java-name "byte"
                                      :array-type (arrc byte-array)}
                      java.lang.Boolean {:java-name "boolean"
                                         :array-type (arrc boolean-array)}
                      java.lang.Character {:java-name "char"
                                           :array-type (arrc char-array)}
                      java.lang.Short {:java-name "short"
                                       :array-type (arrc short-array)}
                      java.lang.Void {:java-name "void"}})

#_(def type-symbols (reduce into {} (map (fn [[k v]]
                                         (let [jn (:java-name v)]
                                           [[(symbol jn) k]
                                            [(symbol (str jn "<>")) (:array-type v)]]))
                                       primitive-types)))

(defn array-class-of-type [tp]
  (class (make-array tp 0)))

#_(def primitive-type-names {java.lang.Float #{:float :float32 :single}
                             java.lang.Double #{:double :float64}
                             })

#_(defn primitive-type? [x]
  (contains? primitive-types x))

#_(defn to-jvm-type [x]
  (if (primitive-type? x)
    x
    java.lang.Object))


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
