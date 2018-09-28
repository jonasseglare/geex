(ns geex.core.utils

  "This is the implementation of the *Polhem* compiler, along with fundamental tools.

  "
  
  (:require [bluebell.utils.wip.party :as party]
            [clojure.spec.alpha :as spec]
            
            [bluebell.utils.wip.core :as utils]
            [clojure.pprint :as pp]
            [clojure.string :as cljstr]
            [bluebell.utils.wip.debug :as debug]
            [clojure.spec.test.alpha :as stest]
            [bluebell.utils.wip.party.coll :as partycoll]
            [geex.debug :refer [set-inspector inspect inspect-expr-map]]
            [bluebell.utils.wip.specutils :as specutils]
            [bluebell.utils.wip.trace :as trace]
            [geex.core.defs :as defs]
            [geex.core.seed :as sd]
            [geex.core.jvm :as gjvm]
            [geex.core.exprmap :as exm]
            [geex.core.datatypes :as datatypes]
            [geex.core.loop :as looputils]
            [geex.core.xplatform :as xp]
            [clojure.set :as cljset]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Definitions and specs
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Phases:
;;
;;  - The user builds a nested datastructure, where some values are seeds
;;      NOTE:
;;        - Symbols represent unknown values
;;  - We traverse the datastructure, and every seed becomes a seed
;;  - We remap the datastructure, assigning a symbol to every seed.
;;  - We build a graph
;;  - We traverse the graph from the bottom, compiling everything.


(def ^:dynamic scope-state nil)

(def contextual-gensym defs/contextual-gensym)

(def contextual-genkey (comp keyword contextual-gensym))

(def contextual-genstring (comp str contextual-gensym))


(defn wrap-expr-compiler [c]
  {:pre [(fn? c)]}
  (fn [comp-state expr cb]
    (cb (defs/compilation-result comp-state (c expr)))))

(def state-defaults {:platform :clojure
                     :disp-total-time? false})

(defn only-non-whitespace? [x]
  (->> x
      vec
      (map str)
      (not-any? cljstr/blank?)))

(def base-seed (-> {}
                   (sd/access-tags #{})
                   (sd/referents #{})
                   (sd/compiler nil)
                   (sd/datatype nil)
                   (defs/access-omit-for-summary [])))

(def access-original-coll (party/key-accessor :original-coll))

;;??
(defn value-literal-type [x]
  (if (symbol? x)
    defs/dynamic-type
    (datatypes/unboxed-class-of x)))


(defn ensure-seed? [x]
  (assert (sd/compilable-seed? x))
  x)

;;;;;; Analyzing an expression 


;;; Helper for flat-seeds-traverse


(def access-bind-symbol (party/key-accessor :bind-symbol))

(defn inherit-datatype [x from]
  (defs/datatype x (defs/datatype from)))

(defn var-symbol [x]
  (-> x :var :name symbol))

;; Normalize something to a type such that we get the same type when we call rest on it.

(xp/register
 :clojure
 {:render-bindings
  (fn [tail body]
    `(let ~(reduce into [] (map (fn [x]
                                  [(:name x) (:result x)])
                                tail))
       ~body))

  :to-variable-name symbol

  :get-type-signature gjvm/get-type-signature
  :get-compilable-type-signature
  gjvm/get-compilable-type-signature

  :compile-coll
  (fn [comp-state expr cb]
    (cb (defs/compilation-result
          comp-state
          (partycoll/normalized-coll-accessor
           (access-original-coll expr)
           (exm/lookup-compiled-indexed-results comp-state expr)))))

  :compile-static-value
  (fn  [state expr cb]
    (cb (defs/compilation-result state (sd/static-value expr))))


  :declare-local-vars
  (fn [comp-state cb]
    (let [vars (::defs/local-vars comp-state)]
      (if (empty? vars)
        (cb comp-state)

        ;; Generate the code for local variables
        `(let ~(transduce
                (comp (map (comp :vars second))
                      cat
                      (map (fn [x] [(-> x :name symbol) `(atom nil)]))
                      cat)
                conj
                []
                vars)
           ~(cb (assoc comp-state ::defs/local-vars {}))))))

  :render-sequential-code
  (fn [code]
    `(do
       ~@code
       nil))

  :compile-bind
  (fn [comp-state expr cb]
    (cb (defs/compilation-result
          comp-state (access-bind-symbol expr))))


  :compile-bind-name
  (fn [x]
    (throw (ex-info "Not applicable for this platform" {:x x})))


  :compile-return-value
  (fn [datatype expr]
    (throw (ex-info "Return value not supported on this platform"
                    {:datatype datatype
                     :expr expr})))

  :compile-nil
  (fn [comp-state expr cb]
    (cb (defs/compilation-result
          comp-state
          nil)))

  :check-compilation-result (constantly nil)
  
  })
