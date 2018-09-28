(ns geex.core.defs
  
  "Common definitions that are shared between different modules of the code."
  
  (:require [bluebell.utils.wip.party :as party]
            [clojure.spec.alpha :as spec]
            [bluebell.utils.wip.specutils :as specutils]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  The state used for book-keeping when generating code.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:dynamic state nil)



(def ^:dynamic gensym-counter nil)

(defn make-gensym-counter []
  (atom 0))


;; Keys are unique within a context. That way, we should always generate the same expression
;; for the same data, and can thus compare values for equality to see if something changed.
(defn contextual-gensym
  ([] (contextual-gensym "untagged"))
  ([prefix0]
   (let [prefix (str prefix0)]
     (assert (not (nil? gensym-counter)))
     (symbol (str "gs-" prefix "-" (swap! gensym-counter inc))))))

(defn new-or-existing-gensym-counter []
  (or gensym-counter
      (make-gensym-counter)))

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

(spec/def ::comp-state (spec/keys :req [::seed-map]))

(spec/def ::platform any?)
(spec/def ::trace-key keyword?)
(spec/def ::base-init (spec/keys :opt-un [::trace-key ::platform]))


(spec/def ::full-seed (spec/keys :req [::type
                                       ::compiler
                                       ::deps]
                                 :opt [::referents]))

(spec/def ::seed ::full-seed)

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

(defn has-compilation-result? [x]
  (contains? x ::compilation-result))

(defn clear-compilation-result [comp-state]
  (dissoc comp-state ::compilation-result))

(defn compiled-seed? [x]
  (contains? x ::compilation-result))

;; Access the dirty-counter
(def dirty-counter (party/key-accessor ::dirty-counter))
(defn dirty? [x]
  (and (seed? x)
       (contains? x ::dirty-counter)))

;; Access the compiltation state as a key in the global state
(def access-comp-state (party/key-accessor ::comp-state))

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
                                       :seedref any?;keyword?
                                       ))


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




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Platforms
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def default-platform :clojure)

(def clojure-platform :clojure)
(def java-platform :java)
(def jvm-bytecode-platform :jvm-bytecode)
(def js-platform :js)


(defn get-platform
  "Get the platform identifier, or :clojure if undefined."
  []
  (cond
    (nil? state) default-platform
    (map? state) (access-platform state)
    :default (access-platform (deref state))))


(defn platform-dispatch
  "This function can be used as a dispatch function when we write platform specific code."
  [& args]
  (get-platform))

(defn get-platform-tag []
  [:platform (get-platform)])





(spec/def ::desc string?)

(spec/def ::dirtified? boolean?)

(spec/def ::ref-tag keyword?)

(spec/def ::scope-spec (spec/keys :req-un [::desc
                                           ::dirtified?]
                                  :opt-un [::ref-tag]))

(spec/def ::name any?)
(spec/def ::result any?)

(spec/def ::binding (spec/keys :req-un [::name
                                        ::result
                                        ::seed]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Referents and deps
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def scope-ref-tag :scope-ref)
(def bind-ref-tag :bind-ref)
(def sideeffect-ref-tag :sideeffect-ref)

(def scope-ref-set #{
                     ;; Used by scopes to control the order of compilation
                     scope-ref-tag
                     })
(spec/def ::invisible-tag-key scope-ref-set)

(def bind-ref-set #{ ;; Used by loops to encourage an expression to be bound outside of
                    ;; the loop
                    bind-ref-tag
                    })
(spec/def ::bind-tag-key bind-ref-set)


(def sideeffect-ref-set #{sideeffect-ref-tag})



;; Three kinds of deps: scope-ref, bind-ref, sideffect-ref and any other
;;

(def simple-tag :simple)

(spec/def ::seed-dep-key (spec/or :composite
                                  (spec/cat :key (spec/or :scope-ref scope-ref-set
                                                          :bind-ref bind-ref-set
                                                          :sideeffect-ref sideeffect-ref-set
                                                          )
                                            :value any?)
                                  :simple any?))
(spec/def ::seed-ref (spec/cat :key ::seed-dep-key
                               :value any?))
(spec/def ::seed-refs (spec/coll-of ::seed-ref))

(spec/def ::bool-or-nil #{true false nil})
(spec/def ::explicity-bind? ::bool-or-nil)
(spec/def ::dirty? boolean?)

(spec/def ::ref-summary (spec/coll-of #{scope-ref-tag
                                        bind-ref-tag
                                        sideeffect-ref-tag
                                        simple-tag}))

(spec/def ::seed-binding-summary (spec/keys :req-un [::explicit-bind?
                                                     ::dirty?
                                                     ::ref-summary]))

(def bind-levels [:dont-bind :list :bind])
(def bind-or-list #{:bind :list})
(def bind-level-map (into {} (map-indexed (comp vec reverse vector) bind-levels)))
(spec/def ::bind-level (set bind-levels))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Misc access of seeds
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def access-method-name (party/key-accessor :method-name))
(def access-class (party/key-accessor :class))
(def access-operator (party/key-accessor :operator))
(def access-name (party/key-accessor :name))
