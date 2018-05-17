(ns lime.platform.low

  "Platform specific code needed by the compiler"
  
  (:require [bluebell.utils.defmultiple :refer [defmultiple defmultiple-extra]]
            [bluebell.utils.core :as utils]
            [lime.core.defs :as defs]
            ;[insn.core :as insn]
            ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Common utilities
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def known-platforms (atom #{:clojure
                             :java}))

(defn known-platform? [x]
  (contains? (deref known-platforms) x))

(defn register-platform
  "When extending this library to support new paltforms (e.g. OpenCL), register it here"
  [new-platform]
  (swap! known-platforms conj new-platform))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Code generators
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmultiple compile-static-value defs/platform-dispatch
  (defs/clojure-platform [value] value)
  (defs/java-platform [value] (str value)))

