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

(def default-platform :clojure)

(def ^:dynamic the-platform default-platform)

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
;;;  Various definitions with no or little logic related to them
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Access the datatype of the seed
(defn datatype [x]
  (.getType x))

(defn compilation-result
  ([state x]
   (.setCompilationResult state x)
   state)
  ([state]
   (.getCompilationResult state)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Platforms
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def clojure-platform :clojure)
(def java-platform :java)
(def jvm-bytecode-platform :jvm-bytecode)
(def js-platform :js)


(defn get-platform
  "Get the platform identifier, or :clojure if undefined."
  []
  the-platform)


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
