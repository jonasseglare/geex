(ns lime.visualize
  (:import [java.awt Desktop])
  (:require [bluebell.utils.core :as utils]
            [clojure.string :as cljstr]
            [lime.core.seed :as sd]
            [lime.core :as lime]
            [clojure.java.io :as io]
            [lime.core.exprmap :as exm]
            [clojure.java.shell :as shell]))

(defn format-for-graphviz [x]
  (cljstr/replace
   (if (or (keyword? x)
           (symbol? x))
     (name x)
     (str x))
   "-" "_"))

(defn gen-graphviz-edges-for-seed [[seed-key seed]]
  (map (fn [[dep-key dep]]
         (str (format-for-graphviz seed-key)
              " -> "
              (format-for-graphviz dep)
              " [label=\"" (format-for-graphviz dep-key) "\"];"))
       (sd/access-deps seed)))

;; http://www.graphviz.org/pdf/dotguide.pdf
(defn expr-map-to-graphviz [em]
  (utils/indent-nested
   ["digraph G {"
    [(str  "top -> " (format-for-graphviz (exm/access-top em)) ";")]
    (map gen-graphviz-edges-for-seed (exm/seed-map em))
    "}"]))

(defn save-expr-map-to-graphviz 
  "Save a graphviz formatted file of the expression map 'em' to 'filename'.
Use the commandline, e.g. 

dot /tmp/m.dot -Tpdf -o kattskit.pdf && evince kattskit.pdf

  to produce a pdf. "
  [filename em]
  (assert (string? filename))
  (spit filename (expr-map-to-graphviz em)))

(defn plot-expr-map [em]
  (let [src-filename "/tmp/lime.dot"
        dst-filename "/tmp/lime.pdf"]
    (save-expr-map-to-graphviz src-filename em)
    (shell/sh "dot" src-filename "-Tpdf" "-o" dst-filename)
    (.open (Desktop/getDesktop) (io/file dst-filename))))
