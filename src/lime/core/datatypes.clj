(ns lime.core.datatypes)

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

(defn query-return-type [f args]
  (let [samples (map sample-type-map args)]
    (try
      (if (every? (complement nil?) samples)
        (class (apply f samples)))
      (catch Throwable e nil))))
