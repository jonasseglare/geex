(ns geex.java.try-block
  (:require [clojure.spec.alpha :as spec]))


(spec/def ::body fn?)
(spec/def ::finally fn?)
(spec/def ::type class?)
(spec/def ::catch (spec/keys :req-un [::type ::body]))
(spec/def ::catches (spec/* ::catch))
(spec/def ::try-block (spec/keys :req-un [::body]
                                 :opt-un [::catches
                                          ::finally]))

(defn validate [x]
  (when (not (spec/valid? ::try-block x))
    (throw (ex-info
            (str "Not a valid try-block: "
                 (spec/explain-str ::try-block x))
            {:block x})))
  x)
