(ns lime.platform.core
  (:require [bluebell.utils.defmultiple :refer [defmultiple defmultiple-extra]]
            [bluebell.utils.core :as utils]
            ;[insn.core :as insn]
            ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Common utilities
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def known-platforms (atom #{:clojure
                             :jvm}))

(defn known-platform? [x]
  (contains? (deref known-platforms) x))

(defn register-platform
  "When extending this library to support new paltforms (e.g. OpenCL), register it here"
  [new-platform]
  (swap! known-platforms conj new-platform))

(defn dispatch-function
  "A function that will dispatch on the platform key of the first argument"
  [& args]
  (let [platform (first args)]
    (utils/data-assert (known-platform? platform)
                       "Unknown platform" {:platform platform
                                           :in args})
    platform))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Code generators
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmultiple compile-static-value dispatch-function
  (:clojure [platform value]
            value))

