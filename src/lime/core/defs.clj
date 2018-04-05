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




(spec/def ::seed (spec/keys :req [::type
                                  ::compiler
                                  ::deps]))

(spec/def ::basic-seed (spec/keys :req [::type]))

(spec/def ::snapshot (spec/keys :req [::result-value
                                      ::last-dirty]))

;; Access result value, of a snapshot type
(def result-value (party/key-accessor ::result-value))

;; Access a backup place for the dirty, when using record-dirties
(def backup-dirty (party/key-accessor ::backup-dirty))

(def snapshot? (partial spec/valid? ::snapshot))

;; Access the last dirty
(def last-dirty (party/key-accessor ::last-dirty))

;; Access the datatype of the seed
(def datatype (party/key-accessor ::type))

;; Test if something is a seed
(defn seed? [x]
  (and (map? x)
       (contains? x ::type)))

(def compilation-result (party/key-accessor ::compilation-result))

(defn clear-compilation-result [comp-state]
  (dissoc comp-state ::compilation-result))

(defn compiled-seed? [x]
  (contains? x ::compilation-result))

;; Access the dirty-counter
(def dirty-counter (party/key-accessor ::dirty-counter))
(defn dirty? [x]
  (and (seed? x)
       (contains? x ::dirty-counter)))

;; Increase the counter of the state map
(def inc-counter #(party/update % dirty-counter inc))
