(ns lime.pc-test
  (:import [BBox]))

(set! *warn-on-reflection* true)

(def pc-width 160)
(def pc-height 120)
(def pixel-count (* pc-height pc-width))
(def elem-size 4)
(defn gen-data [] (float-array (take (* 4 pixel-count)  (repeatedly rand))))

(def more-data (take 100 (repeatedly gen-data)))

(def pc-data (gen-data))





(defn good-compute-bbox [^floats data]
  (let [^int n (quot (alength data) pixel-count)]
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
        i))))

(defmacro my-min [a b]
  `(if (< ~a ~b) ~a ~b))

(defmacro my-max [a b]
  `(if (< ~a ~b) ~b ~a))

(defn lime-compute-bbox [^floats data]
  (let [^int n (quot (alength data) pixel-count)]
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
        i))))

(defn arr-compute-bbox [^floats data]
  (let [^int n (quot (alength data) pixel-count)
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

(defn java-version [x]
  (BBox/computeBBox x))

(defn benchmark-it [compute-it]
  (let [d (vec (take 100 (repeatedly gen-data)))]
    (time (doseq [x d]
            (compute-it x)))))
