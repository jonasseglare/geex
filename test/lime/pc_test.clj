(ns lime.pc-test
  (:require [no.disassemble :as nd]
            [clj-java-decompiler.core :refer [decompile]]
            [proteus :refer [let-mutable]])
  (:import [BBox]))

(set! *warn-on-reflection* true)
;;(set! *unchecked-math* :warn-on-boxed)

(def pc-width 160)
(def pc-height 120)
(def pixel-count (* pc-height pc-width))
(def elem-size 4)
(defn gen-data [] (float-array (take (* 4 pixel-count)  (repeatedly rand))))

(def more-data (take 100 (repeatedly gen-data)))

(def pc-data (gen-data))
(defmacro my-min [a b] `(if (< ~a ~b) ~a ~b))
(defmacro my-max [a b] `(if (< ~a ~b) ~b ~a))




(defn good-compute-bbox [^floats data]
  (let [n (alength data)]
    (loop [i (int 0)
           minx (float (aget data 0))
           maxx (float (aget data 0))
           miny (float (aget data 1))
           maxy (float (aget data 1))
           minz (float (aget data 2))
           maxz (float (aget data 2))]
      (if (< i n)
        (let [x (float (aget data (unchecked-add i 0)))
              y (float (aget data (unchecked-add i 1)))
              z (float (aget data (unchecked-add i 2)))]
          (if (and (Float/isFinite x)
                   (Float/isFinite y)
                   (Float/isFinite z))
            (recur (unchecked-add-int i 4)
                   (min minx x)
                   (max maxx x)
                   (min miny y)
                   (max maxy y)
                   (min minz z)
                   (max maxz z))
            (recur (unchecked-add-int i 4)
                   minx
                   maxx
                   miny
                   maxy
                   minz
                   maxz
                   ))
          )
        [minx maxx miny maxy minz maxz]))))

(defmacro my-min [a b]
  `(if (< ~a ~b) ~a ~b))

(defmacro my-max [a b]
  `(if (< ~a ~b) ~b ~a))

(defn lime-compute-bbox [^floats data]
  (let [n (alength data)]
    (loop [i (long 0)
           minx (float (aget data 0))
           maxx (float (aget data 0))
           miny (float (aget data 1))
           maxy (float (aget data 1))
           minz (float (aget data 2))
           maxz (float (aget data 2))]
      (if (< i n)
        (let [x (float (aget data (unchecked-add i 0)))
              y (float (aget data (unchecked-add i 1)))
              z (float (aget data (unchecked-add i 2)))]
          (let [[^float minx
                 ^float maxx
                 ^float miny
                 ^float maxy
                 ^float minz
                 ^float maxz]
                (if (and (Float/isFinite x)
                         (Float/isFinite y)
                         (Float/isFinite z))
                  [(min minx x)
                   (max maxx x)
                   (min miny y)
                   (max maxy y)
                   (min minz z)
                   (max maxz z)]
                  [minx
                   maxx
                   miny
                   maxy
                   minz
                   maxz])]
            (recur (unchecked-add i 4)
                   minx
                   maxx
                   miny
                   maxy
                   minz
                   maxz)))
        [minx maxx miny maxy minz maxz]))))


(defn arr-compute-bbox [^floats data]
  (let [n (alength data)
        ^floats acc (float-array [(float (aget data 0))
                                  (float (aget data 0))
                                  (float (aget data 1))
                                  (float (aget data 1))
                                  (float (aget data 2))
                                  (float (aget data 2))]
                                 )]
    (loop [i (long 0)
           ]
      (if (< i n)
        (let [x (float (aget data (unchecked-add i 0)))
              y (float (aget data (unchecked-add i 1)))
              z (float (aget data (unchecked-add i 2)))]
          (if (and (Float/isFinite x)
                   (Float/isFinite y)
                   (Float/isFinite z))
            (do
              (aset acc 0 (min (aget acc 0) x))
              (aset acc 1 (max (aget acc 1) x))
              (aset acc 2 (min (aget acc 2) y))
              (aset acc 3 (max (aget acc 3) y))
              (aset acc 4 (min (aget acc 4) z))
              (aset acc 5 (max (aget acc 5) z)))
            )
          (recur (unchecked-add i 4)))
        i))
    acc))

(defmacro update-bds [[minv maxv] v]
  `(do
     (var-set ~minv (min (deref ~minv) ~v))
     (var-set ~maxv (max (deref ~maxv) ~v))))

(defn lvars-compute-bbox [^floats data]
  (let [n (alength data)]
    (with-local-vars [minx (float (aget data 0))
                      maxx (float (aget data 0))
                      miny (float (aget data 1))
                      maxy (float (aget data 1))
                      minz (float (aget data 2))
                      maxz (float (aget data 2))]
      (loop [i (long 0)]
        (if (< i n)
          (let [x (float (aget data (unchecked-add i 0)))
                y (float (aget data (unchecked-add i 1)))
                z (float (aget data (unchecked-add i 2)))]
            (if (and (Float/isFinite x)
                     (Float/isFinite y)
                     (Float/isFinite z))
              (do
                (update-bds [minx maxx] x)
                (update-bds [miny maxy] y)
                (update-bds [minz maxz] z))
              )
            (recur (unchecked-add i 4)))
          i))
      [(deref minx)
(deref maxx)
(deref miny)
(deref maxy)
(deref minz)
(deref maxz)
       ])))

(defmacro update-mut [[minv maxv] v]
  `(do
     (set! ~minv (min ~minv ~v))
     (set! ~maxv (max ~maxv ~v))))

(defn proteus-compute-bbox [^floats data]
  (let [n (alength data)]
    (let-mutable [^float minx (float (aget data 0))
                  ^float maxx (float (aget data 0))
                  ^float miny (float (aget data 1))
                  ^float maxy (float (aget data 1))
                  ^float minz (float (aget data 2))
                  ^float maxz (float (aget data 2))]
      (loop [i (long 0)]
        (if (< i n)
          (let [x (float (aget data (unchecked-add i 0)))
                y (float (aget data (unchecked-add i 1)))
                z (float (aget data (unchecked-add i 2)))]
            (if (and (Float/isFinite x)
                     (Float/isFinite y)
                     (Float/isFinite z))
              (do
                (update-mut [minx maxx] x)
                (update-mut [miny maxy] y)
                (update-mut [minz maxz] z))
              )
            (recur (unchecked-add i 4)))
          i))
      [minx
       maxx
       miny
       maxy
       minz
       maxz
       ])))

(defn java-version [x]
  (BBox/computeBBox x))

(defn benchmark-it [compute-it]
  (let [d (vec (take 100 (repeatedly gen-data)))]
    (time (doseq [x d]
            (compute-it x)))))

(defmacro benchmark-many [fns]
  `(do
     (doseq [[label#  f#] ~(mapv vector (map str fns) fns)]
       (println label#)
       (benchmark-it f#))))


(comment
  (benchmark-many [java-version
                   good-compute-bbox
                   arr-compute-bbox
                   proteus-compute-bbox
                   lime-compute-bbox
                   lvars-compute-bbox ])
  java-version
"Elapsed time: 21.385044 msecs"
good-compute-bbox
"Elapsed time: 105.513528 msecs"
arr-compute-bbox
"Elapsed time: 171.804541 msecs"
proteus-compute-bbox
"Elapsed time: 237.169185 msecs"
lime-compute-bbox
"Elapsed time: 416.293138 msecs"
lvars-compute-bbox
"Elapsed time: 2547.549587 msecs"
  nil)


