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

(def access-original-coll (party/key-accessor :original-coll))


(def access-bind-symbol (party/key-accessor :bind-symbol))

(defn inherit-datatype [x from]
  (defs/datatype x (defs/datatype from)))

(defn var-symbol [x]
  (-> x :var :name symbol))

