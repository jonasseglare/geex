(ns geex.java.reflect
  (:require [bluebell.utils.wip.pareto :as pareto]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Inteface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def boxed-primitive-classes
  [[Character Character/TYPE]
   [Boolean Boolean/TYPE]
   [Byte Byte/TYPE]
   [Short Short/TYPE]
   [Integer Integer/TYPE]
   [Long Long/TYPE]
   [Float Float/TYPE]
   [Double Double/TYPE]])

(defn boxes-or-unboxes-to? [a b]
  (some
   (fn [[cl ucl]]
     (or (and (= cl a) (= ucl b))
         (and (= ucl a) (= cl b))))
   boxed-primitive-classes))

(defn arg-matches? [a b]
  (or (isa? a b)
      (boxes-or-unboxes-to? a b)))

(defn arglist-matches? [query-arglist available-arglist]
  {:pre [(coll? query-arglist)
         (coll? available-arglist)]}
  (and (= (count query-arglist)
          (count available-arglist))
       (every?
        identity
        (map arg-matches? query-arglist available-arglist))))

(defn matching-method? [method-name arg-types method]
  (and (= method-name (.getName method))
       (let [ptypes (vec (.getParameterTypes method))]
         (arglist-matches? arg-types ptypes))))

(defn list-matching-methods [cl method-name arg-types]
  {:pre [(class? cl)
         (string? method-name)
         (coll? arg-types)
         (every? class? arg-types)]}
  (filter
   (partial matching-method? method-name arg-types)
   (.getMethods cl)))

(defn dominates-score [a b]
  (if (arg-matches? a b)
    (if (arg-matches? b a)
      0
      1)
    (if (arg-matches? b a)
      -1
      nil)))

(defn method-dominates? [a b]
  (let [a-args (.getParameterTypes a)
        b-args (.getParameterTypes b)]
    (assert (= (count a-args)
               (count b-args)))
    (let [scores (mapv dominates-score a-args b-args)]
      (and (not (some nil? scores)) ;; no unclear relations
           (some (partial = 1) scores) ;; some args that dominate
           (not (some (partial = -1) scores)))))) ;; no args being dominated.

(defn get-matching-method [cl method-name arg-types]
  (let [frontier
        (pareto/elements
         (transduce
          (filter
           (partial matching-method? method-name arg-types))
          (completing pareto/insert)
          (pareto/frontier method-dominates?)
          (.getMethods cl)))
        n (count frontier)]
    (if (or (= 0 n)
            (<= 2 n))
      (throw (ex-info
              (if (= 0 n)
                "No matching method found. Did you consider specifying the return type using :ret? Only then can the calling code know what to expect."
                "Ambiguous method resolution")
              {:class cl
               :method-name method-name
               :arg-types arg-types
               :methods frontier}))
      (first frontier))))



