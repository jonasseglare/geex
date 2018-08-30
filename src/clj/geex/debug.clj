(ns geex.debug

  "Tools used to debug this library."
  
  (:require [bluebell.utils.debug :as debug]
            [bluebell.utils.data-factors :as df]))

(defn basic-inspect [x]
                                        ;(df/disp x)
  (debug/limited-pprint x)
  )

(def inspector (atom basic-inspect))
(def em-inspector (atom basic-inspect))

(defn inspect [x]
  ((deref inspector) x)
  x)

(defn inspect-expr-map [em]
  (let [insp (deref em-inspector)]
    (println "The inspector is" insp)
    (assert (fn? insp))
    (insp em)
    (println "Called the inspector")
    )
  em)

(defn set-inspector [x]
  (reset! inspector x))

(defn set-expr-map-inspector [x]
  (reset! em-inspector x))
