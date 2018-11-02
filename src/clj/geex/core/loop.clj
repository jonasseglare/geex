(ns geex.core.loop
  (:require [clojure.spec.alpha :as spec]))

(spec/def ::binding (spec/cat :vars any?
                              :expr any?))

(spec/def ::loop-args (spec/cat :bindings (spec/spec
                                           (spec/* ::binding))
                                :body (spec/* any?)))

(defn parse-loop-args [args]
  (let [parsed (spec/conform ::loop-args args)]
    (if (= parsed ::spec/invalid)
      (throw (ex-info
              (str "Failed to parse loop arguments: "
                   (spec/explain-str ::loop-args args))))
      parsed)))
