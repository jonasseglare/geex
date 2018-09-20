(ns geex.ebdm.type
  (:require [bluebell.utils.ebmd :as ebdm]
            [bluebell.utils.ebmd.ops :as ops]
            [bluebell.utils.ebmd.type :as type]
            [geex.core.seed :as seed]))

(ebdm/def-arg-spec seed-with-class
  {:pred #(and (seed/seed? %)
               (class? (seed/datatype %)))
   :pos [(seed/typed-seed java.lang.Appendable)]
   :neg []})

(ebdm/def-arg-spec class-arg
  {:pred class?
   :pos [(class 3.0)]
   :neg [3.0]})


