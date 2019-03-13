(ns sample-project.core
  (:require [geex.core :as gx]
            [geex.java :as java]
            [geex.common :as c])
  (:gen-class))

(java/def-class scaler-type
  {:name "Scaler"
   :package "sample.project"

   :variables [{:name "factor"
                :type Double/TYPE}]

   :methods [{:name "scale"
              :ret Double/TYPE
              :arg-types [Double/TYPE]
              :fn (fn [this x]
                    (gx/set-flag! :format)
                    (c/+ 1000 (c/* (this "factor") x)))}]})

(defn make-scaler [factor]
  (let [dst (.newInstance scaler-type)]
    (set! (.factor dst) factor)
    dst))

(java/def-class kattskit {:name "Kattskit"
                          ;;:mode :production
                          :methods [{:name "wrap"
                                     :arg-types [String]
                                     :fn (fn [this x]
                                           (gx/set-flag! :disp-time)
                                           {:x x})}]})

(defn -main [& args]
  (println "Wrapping the first arg...")
  (let [inst (.newInstance kattskit)]
    (println "It is wrapped as" (.wrap inst (first args)))))


