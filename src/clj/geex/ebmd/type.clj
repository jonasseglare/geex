(ns geex.ebmd.type
  (:require [bluebell.utils.ebmd :as ebmd]
            [bluebell.utils.ebmd.ops :as ops]
            [bluebell.utils.ebmd.type :as type]
            [geex.core.seed :as seed]
            [geex.core.defs :as defs]
            [geex.core.datatypes :as datatypes]))

(defn seed-of-type-such-that [pred pos neg]
  {:pred #(and (seed/seed? %)
               (pred (seed/datatype %)))
   :pos pos
   :neg neg})

(ebmd/def-arg-spec seed-with-class
  (seed-of-type-such-that class?
                          [(seed/typed-seed (class "asdf"))]
                          [(class "asdf")]))

(defn seed-of [stype]
  (ebmd/normalize-and-check-arg-spec
   (merge {:key [::seed-of stype]}
          (seed-of-type-such-that (partial = stype)
                                  [(seed/typed-seed stype)]
                                  [(seed/typed-seed ::kattskit)]))))

(def nothing-seed (seed-of ::defs/nothing))



(ebmd/def-arg-spec class-arg
  {:pred class?
   :pos [(class 3.0)]
   :neg [3.0]})

(ebmd/def-arg-spec compilable-seed
  {:pred seed/compilable-seed?
   :pos [(seed/compiler (seed/typed-seed Double/TYPE)
                        (fn [state expr cb]))
         (seed/compiler (seed/typed-seed :kattskit)
                        (fn [state expr cb]))]
   :neg [(seed/typed-seed Double/TYPE)
         (seed/typed-seed :kattskit)]})

(defn map-with-key-value [key value]
  (ebmd/normalize-and-check-arg-spec
   {:pred #(and (map? %)
                (=  (get % key) value))
    :key [::map-with-key-value key value]
    :pos [{key value}]
    :neg [{}]}))

(def array-seed
  (ebmd/normalize-arg-spec
   (merge {:key ::array-seed}
          (seed-of-type-such-that
           datatypes/array-class?
           [(seed/typed-seed
             (datatypes/array-class
              Double/TYPE))]
           [(seed/typed-seed Double/TYPE)]))))

(ebmd/def-arg-spec maybe-seed-of-integer
  {:pred #(or (int? %)
              (and (seed/seed? %)
                   (contains? (set datatypes/integer-types)
                              (seed/datatype %))))
   :pos [1 2 3 (seed/typed-seed Integer/TYPE)]
   :neg [3.4 (seed/typed-seed Double/TYPE)]})
