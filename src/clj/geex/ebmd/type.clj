(ns geex.ebmd.type
  (:require [bluebell.utils.ebmd :as ebmd]
            [bluebell.utils.ebmd.ops :as ops]
            [bluebell.utils.ebmd.type :as type]
            [geex.core.seed :as seed]))

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

(ebmd/def-arg-spec class-arg
  {:pred class?
   :pos [(class 3.0)]
   :neg [3.0]})


