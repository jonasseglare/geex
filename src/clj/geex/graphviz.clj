(ns geex.graphviz
  (:import [java.io File]
           [java.awt Desktop])
  (:require [clojure.spec.alpha :as spec]
            [bluebell.utils.wip.specutils :as specutils]
            [clojure.java.io :as io]
            [clojure.string :as cljstr]
            [clojure.reflect :as reflect]
            [clojure.java.shell :as shell]))

(spec/def ::out-graphviz string?)
(spec/def ::out-pdf string?)
(spec/def ::disp? boolean?)
(spec/def ::lower number?)
(spec/def ::upper number?)
(spec/def ::fontname string?)

(spec/def ::settings
  (spec/keys :req-un []
             :opt-un [::out-graphviz
                      ::lower
                      ::upper
                      ::out-pdf
                      ::disp?
                      ::fontname]))

(defn get-lower [state settings]
  (or (:lower settings) 0))

(defn get-upper [state settings]
  (or (:upper settings) (.getUpper state)))

(defn get-out-graphviz [settings]
  (or (:out-graphviz settings)
      (.getAbsolutePath
       (File/createTempFile "geex" ".dot"))))

(defn generate-out-pdf-file [file]
  {:pre [(string? file)]}
  (let [p file
        i (cljstr/last-index-of p ".")]
    (str
     (if (nil? i)
       p
       (str (subs p 0 i)))
     ".pdf")))

(defn get-out-pdf [out-graphviz settings]
  (or (:out-pdf settings)
      (generate-out-pdf-file out-graphviz)))

(defn get-seed-label [seed]
  (str "\""
       (.getId seed)
       "\\n"
       (.getType seed)
       "\\n"
       (.getDescription seed)
       "\""))

(defn get-seed-key [seed]
  (format "seed%d" (.getId seed)))

(defn render-deps-for-seed [seed-index-set seed]
  (let [src (get-seed-key seed)
        deps (-> seed
                 .deps
                 .getMap)
        prev-index (dec (.getId seed))
        add-to-previous? (and (contains? seed-index-set prev-index)
                              (not (contains?
                                    (->> deps
                                         vals
                                         (map #(.getId %))
                                         set)
                                    prev-index)))]
    (into (transduce
           (comp
            (filter (fn [[k v]] (contains? seed-index-set (.getId v))))
            (map (fn [[k v]]
                   (str src " -> " (get-seed-key v)
                        "[label=\"" (str k) "\"]"))))
           conj
           []
           deps)
          (if add-to-previous?
            [src " -> " (str "seed" prev-index)]
            []))))

(defn font-name-setting [what fn]
  (str "\n" what " [fontname=\"" fn "\"]\n"))

;; getId, getType, getDescription
(defn get-graphviz-code [state lower upper settings]
  (let [seed-range (range lower upper)
        seeds (map #(.getSeed state %) seed-range)
        seed-code (cljstr/join "\n"
                               (map
                                (fn [seed]
                                  (str "\t"(get-seed-key seed)
                                       " [label="
                                       (get-seed-label seed)
                                       "]"))
                                seeds))
        dep-code (cljstr/join
                  "\n"
                  (transduce
                   (comp (map (partial render-deps-for-seed
                                       (set seed-range)))
                         cat)
                   conj
                   []
                   seeds))
        font-code (if-let [fn (:fontname settings)]
                    (str (font-name-setting "graph" fn)
                         (font-name-setting "edge" fn)
                         (font-name-setting "node" fn))
                    "")]
    (str "digraph {\n"
         font-code
         "\nrankdir=BT"
         "\n"
         seed-code "\n" dep-code "\n}")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn callback [settings]
  (specutils/validate ::settings settings)
  (fn [data]
    (let [state (:state data)
          lower (get-lower state settings)
          upper (get-upper state settings)
          out-graphviz (get-out-graphviz settings)
          out-pdf (get-out-pdf out-graphviz settings)
          disp? (let [d (:disp? settings)]
                  (or d (nil? d)))
          code (get-graphviz-code state lower upper settings)]
      (spit out-graphviz code)
      (shell/sh "dot" out-graphviz "-T" "pdf" "-o" out-pdf)
      (when disp?
        (.open (Desktop/getDesktop)
               (io/file out-pdf)))
      (println "Out graphviz is" out-graphviz)
      (println "Out pdf is" out-pdf))))
