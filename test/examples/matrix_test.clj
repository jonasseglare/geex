(ns examples.matrix-test
  (:require [clojure.spec.alpha :as spec]
            [clojure.test :refer :all]
            [geex.lib :as l]
            [geex.java :as java]
            [geex.core :as core]))

(spec/def ::rows any?)
(spec/def ::cols any?)
(spec/def ::data any?)
(spec/def ::matrix (spec/keys :req-un [::rows ::cols ::data]))

(def matrix? (partial spec/valid? ::matrix))

(defn compute-index [matrix i j]
  (l/cast Integer/TYPE
          (l/+ i (l/* j (:rows matrix)))))

(defn get-element [matrix i j]
  {:pre [(matrix? matrix)]}
  (l/aget (:data matrix) (compute-index matrix i j)))

(defn set-element [matrix i j x]
  {:pre [(matrix? matrix)]}
  (l/aset (:data matrix)
          (compute-index matrix i j) x))

(defn compute-mat-mul-element [A B i j]
  (l/reduce
   (fn [sum k]
     (l/+ sum
          (l/* (get-element A i k)
               (get-element B k j))))
   (l/wrap 0.0)
   (l/range (:cols A))))

(defn allocate-matrix [rows cols]
  {:rows rows
   :cols cols
   :data (l/make-array
          Double/TYPE
          (l/cast
           Integer/TYPE
           (l/* rows cols)))})

(defn multiply-matrices [A B]
  (let [rows (:rows A)
        cols (:cols B)
        C (allocate-matrix rows cols)]
    (l/doseq [i (l/range rows)]
      (l/doseq [j (l/range cols)]
        (set-element
         C i j
         (compute-mat-mul-element
          A B i j))))
    C))

(defn transpose [A]
  (let [rows (:rows A)
        cols (:cols A)
        dst (allocate-matrix cols rows)]
    (l/doseq [i (l/range rows)]
      (l/doseq [j (l/range cols)]
        (set-element
         dst j i
         (get-element A i j))))
    dst))

(defn squared-element-sum [A]
  (l/reduce
   (fn [sum x]
     (l/+ sum (l/* x x)))
   (l/wrap 0.0)
   (l/sliceable-array (:data A))))

(defn normalize-matrix-elements [A]
  (let [sq-sum (squared-element-sum A)
        norm (l/sqrt sq-sum)
        factor (l// 1.0 norm)
        rows (:rows A)
        cols (:cols A)
        dst (allocate-matrix rows cols)
        ]

    ;;;;; TODO DOESNTB BUILD
    (l/doseq [i (l/range rows)]
      (l/doseq [j (l/range cols)]
        (set-element
         dst
         i j
         (l/* factor
              (get-element A i j)))))
    dst))






;;;------- Testing code -------

(def MatrixType  {:rows Long/TYPE
                  :cols Long/TYPE
                  :data (l/array-class Double/TYPE)})



(def test-mat {:rows 3
               :cols 2
               :data (double-array (range 6))})
; [0 3; 1 4; 2 5]

(def test-mat-2 {:rows 2
                 :cols 3
                 :data (double-array
                        [1 9 3 4 4 5])})
; [1 3 4; 9 4 5]

(java/typed-defn sq-elem-sum-fn [MatrixType x]
                 (squared-element-sum x))

(set! *print-length* nil)

(java/typed-defn normalize-fn [MatrixType x]
                 ;(core/set-flag! :disp-final-state)
                 (normalize-matrix-elements x))

(java/typed-defn transpose-fn [MatrixType X]
  (transpose X))


;; Just to check that get-element works
(java/typed-defn
 get-element-f
 [MatrixType matrix
  Long/TYPE i
  Long/TYPE j]
 (get-element matrix i j))

(java/typed-defn
 dot-product
 [(l/array-class Double/TYPE) A
  (l/array-class Double/TYPE) B]
 (compute-mat-mul-element
  {:rows 1
   :cols (l/count A)
   :data A}
  {:rows (l/count B)
   :cols 1
   :data B}
  0 0))

(java/typed-defn
 mat-mul-fn [MatrixType a
             MatrixType b]
 (multiply-matrices a b))

(deftest various-tests
  (is (= (get-element-f
          {:rows 3
           :cols 2
           :data
           (double-array (range 6))}
          1 1)
         4.0))
  (is (= (vec
          (:data 
           (java/eval
            (let [arr (l/make-array Double/TYPE 3)
                  matrix {:rows 1
                          :cols 3
                          :data arr}]
              (set-element matrix 0 1 119.0)
              matrix))))
         [0.0 119.0 0.0]))
  (is (= (dot-product (double-array [1 2 3])
                      (double-array [1 2 1]))
         8.0))
  (is (= (sq-elem-sum-fn test-mat)
         55.0))
  (let [m (transpose-fn
           {:rows 1
            :cols 2
            :data (double-array [7 17])})]
    (is (= (:rows m) 2))
    (is (= (:cols m) 1))
    (is (= (-> m :data vec)
           [7.0 17.0])))
  (let [prod (mat-mul-fn test-mat test-mat-2)]
    (is (= 3 (:rows prod)))
    (is (= 3 (:cols prod)))
    (is (= (vec (:data (mat-mul-fn
                        test-mat test-mat-2)))
           [27.0 37.0 47.0 12.0 19.0 26.0
            15.0 24.0 33.0]))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Power method
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Computing the maximum eigenvector of a matrix

(def test-mat-3 (mat-mul-fn
                 test-mat
                 (transpose-fn test-mat)))

(defn power-iteration [A X]
  (let [Y (multiply-matrices A X)
        Yhat (normalize-matrix-elements Y)]
    Yhat))

(defn initialize-power-iter-vec [n]
  (let [dst (allocate-matrix n 1)]
    (l/doseq [i (l/range n)]
      (set-element dst i 0 1.0))
    dst))

(defn power-method [A]
  (second
   (l/iterate-while

    ;; Initial state
    [(l/wrap 0)
     (initialize-power-iter-vec (:rows A))]

    ;; Iteration
    (fn [[counter X]]
      [(l/inc counter)
       (power-iteration A X)])

    ;; Condition for looping
    (fn [[counter _]]
      (l/< counter 12)))))

(java/typed-defn
 power-method-fn [MatrixType A]

 ;;(core/set-flag! :disp-final-source)
 ;;(core/set-flag! :disp-time)
 
 (power-method A))

(deftest power-method-test
  (let [max-vec (vec (:data
                      (power-method-fn
                       test-mat-3)))

        ;; This is the actual
        ;; maximum Eigenvector
        expected [0.392541
                  0.560772
                  0.729004]]
    
    (is (= (count max-vec)
           (count expected)))
    (doseq [[ours truth]
            (map vector max-vec expected)]
      (is (< (Math/abs (- ours truth))
             0.001)))))
