(ns examples.circle-fit-test
  (:require [geex.common :as lib]
            [geex.core :as core]
            [geex.java :as java]
            [clojure.spec.alpha :as spec]
            [clojure.test :refer :all]))

(spec/def ::derivatives (spec/coll-of any?))
(spec/def ::value any?)
(spec/def ::ad (spec/keys :req-un [::value ::derivatives]))

(spec/def ::params (spec/cat :center (spec/* any?)
                             :radius any?))

(def ad? (partial  spec/valid? ::ad))


;;;------- Common operations on automatically differentiable numbers -------
;;; Normally, it would probably make more sense to overload the operators in lib,
;;; such as lib/+, lib/*, lib/-, etc, but here we define them as functions for
;;; the sake of clarity.

(defn add [x y]
  {:pre [(ad? x)
         (ad? y)]
   :post [(ad? %)]}
  {:value (lib/+ (:value x) (:value y))
   :derivatives (map lib/+
                     (:derivatives x)
                     (:derivatives y))})

(defn mul [x y]
  {:pre [(ad? x)
         (ad? y)]
   :post [(ad? %)]}
  {:value (lib/* (:value x) (:value y))
   :derivatives (mapv (fn [dx dy]
                        (lib/+ (lib/* (:value x) dy)
                               (lib/* (:value y) dx)))
                      (:derivatives x)
                      (:derivatives y))})

(defn sqr [x]
  (mul x x))

(defn negate [x]
  {:value (lib/negate (:value x))
   :derivatives (mapv lib/negate (:derivatives x))})

(defn sub [x y]
  (add x (negate y)))

(defn sqrt [x]
  (let [value (lib/sqrt (:value x))]
    {:value value
     :derivatives (mapv (fn [d] (lib/* d (lib// 0.5 value)))
                        (:derivatives x))}))


;; Test the automatic differentiation
(deftest various-ad-tests
  (is (= (java/eval
          (add {:value 3.0
                :derivatives [2 3]}
               {:value 4.0
                :derivatives [5 8]}))
         {:value 7.0 :derivatives [7 11]}))
  (is (= (java/eval
          (mul {:value 3.0
                :derivatives [2 3]}
               {:value 4.0
                :derivatives [5 8]}))
         {:value 12.0 :derivatives [23.0 36.0]})))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Objective function
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn zeros [n]
  (vec (repeat n (lib/wrap 0.0))))

;; A variable automatically differentiable number
(defn variable [x i]
  {:value x
   :derivatives (assoc (zeros 3) i (lib/wrap 1.0))})

;; A constant automatically differentiable number
(defn constant [x]
  {:value x
   :derivatives (zeros 3)})

;; Evaluates the fitness for a single observed circle point
(defn evaluate-point-fit [[cx cy radius] [x y]]
  {:pre [(ad? cx)
         (ad? cy)
         (ad? radius)
         (ad? x)
         (ad? y)]}
  (let [dif-x (sub cx x)
        dif-y (sub cy y)
        dist (sqrt (add (sqr dif-x) (sqr dif-y)))]
    (sqr (sub dist radius))))

;; Wraps the parameter vector as a vector
;; of automatically differentiable numbers
(defn ad-wrap-params [params]
  (mapv variable params (range (count params))))

;; Turns the coordinates of a point into automatically
;; differentiable numbers
(defn ad-wrap-point [pt]
  (mapv constant pt))

;; Wraps the array of x/y pairs as
(defn array-to-pts [arr]
  (lib/wrap-struct-array [(lib/typed-seed Double/TYPE)
                          (lib/typed-seed Double/TYPE)] arr))

;; Evaluates the objective function and returns the gradient
(defn evaluate-objf-gradient [params point-array]
  (:derivatives
   (let [ad-params (ad-wrap-params params)]
     (lib/transduce
      (lib/map (fn [pt]
                 (let [wrapped (ad-wrap-point pt)]
                   (evaluate-point-fit ad-params wrapped))))
      (completing add)
      (constant (lib/wrap 0.0))
      point-array))))




;; Returns a function that performs one iteration in gradient descent.
(defn gradient-stepper [step-size points]
  (fn [params]
    (let [gradient (evaluate-objf-gradient
                    params
                    points)]
      (mapv (fn [p g] (lib/- p (lib/* step-size g)))
            params gradient))))





;;;------- The top level optimization routine -------

(java/typed-defn
 optimize [{:center [Double/TYPE Double/TYPE]
            :radius Double/TYPE} initial-params

           (lib/array-class Double/TYPE) point-array
           
           {:step-size Double/TYPE
            :iterations Long/TYPE} settings]

 
 (core/set-flag! :disp-time)
 
 (let [flat-params (spec/unform ::params initial-params)
       points (array-to-pts point-array)
       step (gradient-stepper (:step-size settings) points)
       opt (lib/iterate-times (:iterations settings)
                              flat-params
                              step)]
   (spec/conform ::params opt)))
;; 2018-10-02
;; --- Time report ---
;; Start: 0.00
;; Evaluated state: 0.234
;; Generated code: 0.351
;; Composed class: 0.352
;; Formatted code: 0.361
;; Compiled it: 0.371

;; Number of seeds: 254
;; Time per seed: 0.0014606301240095002


;; 2018-10-12
;; --- Time report ---
;; Start: 0.00
;; Evaluated state: 0.0210
;; Generated code: 0.0270
;; Composed class: 0.0270
;; Formatted code: 0.0500
;; Compiled it: 0.0650

;; Number of seeds: 257
;; Time per seed: 2.5291851058544353E-4





;;;------- Test data generation -------

(defn uniform-rand [a b]
  (+ a (* (rand) (- b a))))

(defn generate-samples [n noise true-circle]
  {:pre [(int? n)
         (spec/valid? ::params true-circle)]}
  (let [[cx cy] (:center true-circle)
        radius (:radius true-circle)]
    (double-array
     (reduce
      into
      []
      (take
       n
       (repeatedly
        (fn []
          (let [angle (* (rand) 2.0 Math/PI)
                extra (uniform-rand (- noise) noise)
                rad (+ extra radius)
                x (+ cx (* rad (Math/cos angle)))
                y (+ cy (* rad (Math/sin angle)))]
            [x y]))))))))


(def test-params {:center [3 4]
                  :radius 12})

(def test-data (double-array
                [7.144107820956012 -7.322348332791618 3.230223305650983 16.140177639691125 4.424385175093361 -8.007161790032917 12.97253408831454 -2.8110006118856043 6.998682852633783 15.63064600733553 7.13540665807617 15.36914329195822 -8.753210789051327 0.4078942279769384 -6.8362221246073585 -2.446448511479189 11.245522096884292 -4.411810894431028 -9.212521320220691 4.065980192518594]))



;;;------- Testing circle fitting with gradient descent -------


;; For testing the gradient calculation
(java/typed-defn
 eval-grad-fn
 [(lib/array-class Double/TYPE) arr]
 ;(core/set-flag! :disp :disp-state)
 (let [wrapped-arr (array-to-pts arr)]
   (evaluate-objf-gradient
    [1.0 2.0 5.0]
    wrapped-arr)))

(deftest basic-tests
  (is (ad? 
       (java/eval
        (evaluate-point-fit (ad-wrap-params [1.0 2.0 1.5])
                            (ad-wrap-point [5.0 5.0])))))
  (is (= (eval-grad-fn (double-array [4 6]))
         [0.0 0.0 0.0]))
  (is (not= (eval-grad-fn (double-array [4 7]))
            [0.0 0.0 0.0]))
  (let [params (optimize {:center [0.0 0.0]
                          :radius 1.0}
                         (double-array [-1 -1
                                        1 1
                                        -1 1
                                        1 -1])
                         {:step-size 0.1
                          :iterations 10})]
    (is (< (Math/abs (- (:radius params)
                        (Math/sqrt 2.0)))
           1.0e-5)))
  (let [params (optimize 
                {:center [0.0 0.0]
                 :radius 1.0}
                test-data
                {:step-size 0.01
                 :iterations 100})
        [cx cy] (:center params)
        radius (:radius params)]
    (is (< (Math/abs (- cx 3)) 0.2))
    (is (< (Math/abs (- cy 4)) 0.2))
    (is (< (Math/abs (- radius 12)) 0.2))))
