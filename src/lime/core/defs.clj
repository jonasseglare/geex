(ns lime.core.defs
  (:require [bluebell.utils.party :as party]
            [clojure.spec.alpha :as spec]))

(def ^:dynamic gensym-counter nil)


;; Keys are unique within a context. That way, we should always generate the same expression
;; for the same data, and can thus compare values for equality to see if something changed.
(defn contextual-gensym
  ([] (contextual-gensym "untagged"))
  ([prefix0]
   (let [prefix (str prefix0)]
     (assert (not (nil? gensym-counter)))
     (symbol (str "gs-" prefix "-" (swap! gensym-counter inc))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Debug flags
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:dynamic debug-seed-order false)


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

;; Special access to a dirty, if any
(def dirty (party/key-accessor ::dirty))

(spec/def ::dirty-key (spec/cat :prefix (partial = ::dirty)
                                :sym symbol?))

(defn dirty-key? [x]
  (spec/valid? ::dirty-key x))

(spec/def ::requirement (spec/cat :tag keyword?
                                  :data (constantly true)))

;; Access the requirements
(def requirements (party/key-accessor ::requirements))

(def requirement-tag (party/index-accessor 0))
(def requirement-data (party/index-accessor 1))

;; The dependencies of a seed
(def access-deps (party/key-accessor ::deps))

(def access-compiled-deps (party/key-accessor ::compiled-deps))




(spec/def ::key-seedref-pair (spec/cat :key (constantly true)
                                       :seedref keyword?))


(spec/def ::referents (spec/coll-of ::key-seedref-pair))

;; The opposite of deps
(def referents (party/key-accessor ::referents))

(def empty-comp-state {:platform :clojure
                       ::seed-map {}})

;; The compiler of a seed
(def compiler (party/key-accessor ::compiler))

(def access-pretweak (party/key-accessor ::pretweak))

(defn pretweak? [x]
  (contains? x ::pretweak))

(def description (party/key-accessor ::description))

(def access-platform (party/key-accessor :platform))

(def access-bind? (party/key-accessor ::bind? {:req-on-get false}))

(def access-to-compile (party/key-accessor ::to-compile))

(def access-seed-key (party/key-accessor ::seed-key))


(def access-tags (party/key-accessor ::tags))

(def default-platform :clojure)


(spec/def ::desc string?)

(spec/def ::dirtified? boolean?)

(spec/def ::flush-root boolean?)

(spec/def ::scope-spec (spec/keys :req-un [::desc
                                           ::dirtified?
                                           ::flush-root?]))

(spec/def ::binding (spec/cat :symbol any?
                              :expr any?))
