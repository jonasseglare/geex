(ns geex.State-test
  (:import [geex
            State
            StateSettings
            ClojurePlatformFunctions]
           [clojure.lang PersistentHashMap])
  (:require [clojure.test :refer :all]
            [bluebell.utils.wip.java :as jutils :refer [set-field]]))

(deftest construct-state-test
  (let [state (State.
               (doto (StateSettings.)
                 (set-field platform :clojure)
                 (set-field generateSeedSymbol (fn [] ;;TODO
                                                 ))
                 (set-field closeScope (fn [] (assert false)))))]
    (is state)
    (is (.isEmpty state))))
