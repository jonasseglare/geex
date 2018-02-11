(ns lime.impl.samplevalues)

(defn add-sample-type [dst x]
  (assoc dst (class x) x))

(def sample-type-map (reduce add-sample-type
                             {}
                             [34.0
                              34
                              (bigint 34)
                              (bigdec 34.0)
                              3/4
                              (byte 3)
                              (short 3)
                              (int 3)
                              \a]))

(defn query-return-type [f args]
  (let [samples (map sample-type-map args)]
    (if (every? (complement nil?) samples)
      (class (apply f samples)))))
