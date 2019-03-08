(ns geex.core.utils)

(defn partial-wrapping-args [f pre-defined & arg-wrappers]
  (fn [& args]
    {:pre [(fn? f)
           (sequential? pre-defined)
           (every? fn? arg-wrappers)]}
    (when (not= (count args)
                (count arg-wrappers))
      (throw (ex-info "The number of arguments does not match the number of argument wrappers"
                      {:f f
                       :predefined pre-defined
                       :args args
                       :arg-wrappers arg-wrappers})))
    (apply f (into (vec pre-defined)
                   (map (fn [w x] (w x))
                        arg-wrappers
                        args)))))

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

(defn environment []
  {:mode (keyword (System/getProperty "geex_mode"))
   :java-output-path (System/getProperty "geex_java_output_path")})
