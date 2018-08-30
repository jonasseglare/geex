(ns geex.debug

  "Tools used to debug this library."
  
  (:require [bluebell.utils.debug :as debug]
            [bluebell.utils.data-factors :as df]
            [geex.visualize :as viz]))

(defn basic-inspect [x]
                                        ;(df/disp x)
  (debug/limited-pprint x)
  )

(def inspector (atom basic-inspect))

(defn plot-expr-map [em]
  (try
    (viz/plot-expr-map em)
    (catch Throwable e
      (println "Failed to plot graph, see exception!!!")
      (throw e))))

(def em-inspector (atom plot-expr-map))

(defn inspect [x]
  ((deref inspector) x)
  x)

(defn inspect-expr-map [em]
  (let [insp (deref em-inspector)]
    (assert (fn? insp))
    (insp em)
    )
  em)

(defn set-inspector [x]
  (reset! inspector x))

(defn set-expr-map-inspector [x]
  (reset! em-inspector x))
