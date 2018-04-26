(ns lime.java

  "Generation of Java backed code"

  (:require [lime.java.defs :as jdefs]
            [lime.core :as lime]
            [clojure.spec.alpha :as spec]
            [bluebell.utils.specutils :as specutils])
  (:import [org.codehaus.janino SimpleCompiler]))

(defn janino-cook-and-load
  "Dynamically compile and load Java code as a class"
  [class-name source-code]
  (let [sc (SimpleCompiler.)]
    (.cook sc source-code)
    (let [cl (.loadClass (.getClassLoader sc) class-name)]
      (.newInstance cl))))


(defmacro typed-defn [& args0]
  (let [args (specutils/force-conform ::jdefs/defn-args args0)]
    (str args)))
