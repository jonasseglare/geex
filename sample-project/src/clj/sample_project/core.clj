(ns sample-project.core
  (:require [geex.core :as gx]
            [geex.java :as java]
            [geex.common :as c])
  (:gen-class))

#_(println "MACRO EXP:"
         (macroexpand
          '(java/def-class
             kattskit
             {:name "Kattskit" 
              :methods [{:name "wrap"
                         :arg-types [String]
                         :fn (fn [this x]
                               (gx/set-flag! :disp-time)
                               {:x x})}]})))

(java/def-class kattskit {:name "Kattskit" 
                          :methods [{:name "wrap"
                                     :arg-types [String]
                                     :fn (fn [this x]
                                           (gx/set-flag! :disp-time)
                                           {:x x})}]})

(defn -main [& args]
  (println "Wrapping the first arg...")
  (let [inst (.newInstance kattskit)]
    (println "It is wrapped as" (.wrap inst (first args)))))


