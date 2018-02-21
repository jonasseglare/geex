(ns lime.pc-test)

(set! *warn-on-reflection* true)

(def pc-width 160)
(def pc-height 120)
(def pixel-count (* pc-height pc-width))
(def elem-size 4)
(def pc-data (float-array (take (* 4 pixel-count)  (repeatedly rand))))

(defn compute-bbox [^floats data]
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
                   maxz)))
        i))))
