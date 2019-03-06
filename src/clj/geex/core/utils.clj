(ns geex.core.utils)

(defn arity-partial [& all-args]
  {:pre [(<= 2 (count all-args))]}
  (let [[f & args0] all-args
        args (vec (butlast args0))
        arity-arg (last args0)
        _ (assert (or (sequential? arity-arg)
                      (set? arity-arg)))]
    (if (set? arity-arg)
      (let [arities arity-arg]
        (fn [& input]
          (let [args (into args input)]
            (when (not (contains? arities (count args)))
              (throw (ex-info
                      "Function called with wrong number of arguments"
                      (merge {:f f
                              :args args
                              :acceptable-arities arities}))))
            (apply f args))))
      (let [arity-count (count arity-arg)]
        (fn [& input]
          (when (not= (count input) arity-count)
            (throw (ex-info "Bad number of arguments provided to partial function"
                            {:f f
                             :base-args args
                             :extra-args input
                             :expected arity-arg})))
          (apply f (into args input)))))))
