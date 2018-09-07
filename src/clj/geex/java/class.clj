(ns geex.java.class
  (:require [clojure.spec.alpha :as spec]
            [bluebell.utils.dsl :as dsl]
            [bluebell.utils.debug :as dbg]
            [bluebell.utils.specutils :as specutils]))


(declare public)


(spec/def ::extends sequential?)
(spec/def ::implements sequential?)
(spec/def ::methods sequential?)
(spec/def ::variables sequential?)
(spec/def ::name string?)

(spec/def ::accumulator (spec/keys :req-un [::extends
                                            ::implements
                                            ::methods
                                            ::variables]
                                   :opt-un [::name]))

(def visibilities #{:public :private :protected})
(spec/def ::visibility visibilities)
(spec/def ::static? boolean?)
(spec/def ::context (spec/keys :req-un [::visibility ::static?]))

(def eval-dsl (dsl/dsl-evaluator {:accumulator-spec ::accumulator
                                  :context-spec ::context}))

(def context? (specutils/pred ::context))
(def accumulator? (specutils/pred ::accumulator))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Implementation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def empty-context {:static? false
                    :visibility :public})
(def empty-accumulator
  {:extends []
   :implements []
   :methods []
   :variables []})

(defn class-spec-sub [name-str body]
  (eval-dsl empty-context
            (assoc empty-accumulator :name name-str)
            body))

(defn with-visibility [v & body]
  {:pre [(spec/valid? ::visibility v)]}
  (fn [c a]
    (eval-dsl
     (assoc c :visibility v)
     a
     body)))

(def private (partial with-visibility :private))
(def public (partial with-visibility :public))
(def protected (partial with-visibility :protected))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro class-spec [name-symbol & body]
  {:pre [(symbol? name-symbol)]}
  `(class-spec-sub ~(str name-symbol) ~(vec body)))
