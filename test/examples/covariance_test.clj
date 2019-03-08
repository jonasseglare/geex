(ns examples.covariance-test
  (:require [geex.common :as c]
            [geex.java :as java]
            [geex.core :as gx]
            [clojure.test :refer :all]))

(defn column-vector-from-array [arr]
  {:rows (c/count arr)
   :cols 1
   :get (fn [i j] (c/aget arr i))})

(defn numel [mat]
  (c/* (:rows mat)
       (:cols mat)))

(defn compute-index [rows i j]
  (c/+ i (c/* rows j)))

(defn realize [mat]
  "Evalutes all the elements of the input matrix and puts them in an array, then forms a new matrix referring to that array"
  (let [n (numel mat)
        dst-array (c/make-array Double/TYPE n)
        rows (:rows mat)
        cols (:cols mat)
        get-element (:get mat)]
    (c/doseq [i (c/range rows)]
      (c/doseq [j (c/range cols)]
        (c/aset dst-array (compute-index rows i j) (get-element i j))))
    {:rows rows
     :cols cols
     :data dst-array
     :get (fn [i j] (c/aget dst-array (compute-index rows i j)))}))

(defn export
  "Remove the :get function, because code cannot be generated from functions"
  [mat]
  (dissoc mat :get))


(defn reshape [column-matrix new-rows]
  "Changes a shape of a column matrix"
  (let [new-cols (c// (:rows column-matrix) new-rows)
        g (:get column-matrix)]
    {:rows new-rows
     :cols new-cols
     :get (fn [i j] (g (c/+ i (c/* j new-rows)) 0))}))

(defn transpose [mat]
  (let [g (:get mat)]
    {:rows (:cols mat)
     :cols (:rows mat)
     :get (fn [i j] (g j i))}))

(defn sub-mat [a b]
  (let [ga (:get a)
        gb (:get b)]
    {:rows (:rows a)
     :cols (:cols a)
     :get (fn [i j] (c/- (ga i j) (gb i j)))}))

(defn mul-mat [a b]
  (c/check (c/= (:cols a) (:rows b)) "Not matching")
  (let [ga (:get a)
        gb (:get b)]
    {:rows (:rows a)
     :cols (:cols b)
     :get (fn [i j]
            (c/transduce
             (c/map (fn [k] (c/* (ga i k) (gb k j))))
             c/+
             0.0
             (c/range (:cols a))))}))

(defn ones [rows cols]
  {:rows rows
   :cols cols
   :get (constantly 1.0)})

(defn scale-mat [scale mat]
  (update mat :get (fn [g] (fn [i j] (c/* scale (g i j))))))

(java/typed-defn covariance-matrix [Long/TYPE vector-dim
                                    (c/array-class Double/TYPE) vector-data]
                 (let [V (column-vector-from-array vector-data)
                       X (reshape V vector-dim)
                       N (c// (c/count vector-data) vector-dim)
                       mu (realize
                           (scale-mat (c// 1.0 N)
                                      (mul-mat (ones 1 N)
                                               (transpose X))))
                       mu-repeated (transpose (mul-mat (ones N 1) mu))
                       Xc (sub-mat X mu-repeated)
                       covariance (scale-mat (c// 1.0 (c/- N 1))
                                             (mul-mat Xc (transpose Xc)))]
                   (-> covariance
                       realize
                       export)))

(defn run [problem]
  (covariance-matrix (:dim problem) (:data problem)))


(def test-data [0.5620938465270469 0.20255148746782325 0.9304891585463975 0.5976239522459676 0.602334851715159 0.8828064344295988])

;; Octave
;; >> v = [0.5620938465270469 0.20255148746782325; 0.9304891585463975 0.5976239522459676; 0.602334851715159 0.8828064344295988]
;; v =

;;    0.56209   0.20255
;;    0.93049   0.59762
;;    0.60233   0.88281

;; >> cov(v)
;; ans =

;;    0.040837   0.013222
;;    0.013222   0.116693

(def expected [0.040837   0.013222 0.013222   0.116693])


(deftest covariance-matrix-test
  (let [computed-covariance (vec (:data (covariance-matrix 2 (double-array test-data))))]
    (is (= (count computed-covariance)
           (count expected)))
    (doseq [[a b] (map vector computed-covariance expected)]
      (is (< (Math/abs (- a b)) 1.0e-5)))))
