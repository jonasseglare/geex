(ns lime.core.datatypes
  (:require [clojure.spec.alpha :as spec]))

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

(def primitive-types {java.lang.Float {:java-name "float"}
                      java.lang.Double {:java-name "double"}
                      java.lang.Integer {:java-name "int"}
                      java.lang.Long {:java-name "long"}
                      java.lang.Byte {:java-name "byte"}
                      java.lang.Boolean {:java-name "boolean"}
                      java.lang.Character {:java-name "char"}
                      java.lang.Short {:java-name "short"}
                      java.lang.Void {:java-name "void"}})

#_(def primitive-type-names {java.lang.Float #{:float :float32 :single}
                             java.lang.Double #{:double :float64}
                             })

(defn primitive-type? [x]
  (contains? primitive-types x))

(defn to-jvm-type [x]
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
