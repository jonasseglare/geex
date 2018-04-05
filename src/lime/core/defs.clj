(ns lime.core.defs
  (:require [bluebell.utils.party :as party]
            [clojure.spec.alpha :as spec]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Various definitions with no or little logic related to them
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Special type that we use when we don't know the type
(def dynamic-type ::dynamic)

(def access-omit-for-summary (party/key-accessor ::omit-for-summary))

(spec/def ::comp-state (spec/keys :req [::result
                                        ::seed-map]))

(spec/def ::platform any?)
(spec/def ::trace-key keyword?)
(spec/def ::base-init (spec/keys :opt-un [::trace-key ::platform]))
