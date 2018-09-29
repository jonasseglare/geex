(ns examples.ad-test
  (:require [geex.lib :as lib]
            [geex.core :as core]
            [geex.java :as java]
            [clojure.test :refer :all]))


;;;------- Constructing ad numbers -------
(defn ad [x dx]
  {:x x :dx dx})

(defn variable [x]
  (ad x 1.0))

(defn constant [x]
  (ad x 0.0))


;;;------- Common operations -------

(defn add [a b]
  {:x (lib/+  (:x a) (:x b))
   :dx (lib/+ (:dx a) (:dx b))})

(defn sub [a b]
  {:x (lib/- (:x a) (:x b))
   :dx (lib/- (:dx a) (:dx b))})

(defn mul [a b]
  {:x (lib/* (:x a) (:x b))
   :dx (lib/+ (lib/* (:x a) (:dx b))
              (lib/* (:dx a) (:x b)))})

(defn sqr [x]
  (mul x x))

(defn div [a b]
  {:x (lib// (:x a) (:x b))
   :dx (lib// (lib/- (lib/* (:dx a) (:x b))
                     (lib/* (:x a) (:dx b)))
              (lib/sqr (:x b)))})

(defn pow [x n]
  {:pre [(number? n)]}
  {:x (lib/pow (:x x) n)
   :dx (lib/* n (lib/pow (:x x) (dec n)))})


;;;------- Basic tests of operations -------

(deftest test-the-ops
  (is (= {:x 7 :dx 2.0}
         (java/eval (add (variable 3) (variable 4)))))
  (is (= {:x -1 :dx 0.0}
         (java/eval
          (sub (variable 3) (variable 4)))))
  (is (= (java/eval (pow (variable 3.0) 2.0))
         (java/eval (sqr (variable 3.0)))))
  (is (= {:x 9
          :dx 6.0}
         (java/eval
          (sqr (variable 3))))))



;;;------- Computing the square root -------
;; using Newton-Raphson

;; Let f(x) = x^2 - k

;; The square root of k is th solution of f(x) = 0
;; f'(x) = 2x

;; We will do this: x_{n+1} = x_{n} - (x_{n}^2 - k)/2*x_{n}

(defn sqrt-iteration [k x]
  (sub x (div (sub (sqr x) k)
              (mul (constant 2) x))))

(defn iterate-sqrt [n k x]
  (last (take n (iterate (partial sqrt-iteration k) x))))


;; Unrolling the loop
(java/typed-defn sqrt-with-derivative [Double/TYPE x]
                 (iterate-sqrt 10 (variable x) (constant x)))

(defn iterate-sqrt2 [n k x]
  (lib/iterate-while
   [(lib/wrap 0) (add x (variable (lib/wrap 0.0)))]
   (fn [[counter est]]
     [(lib/inc counter)
      (sqrt-iteration k est)])
   (fn [[counter _]]
     (lib/< counter 10))))


;; Using a loop
(java/typed-defn
 sqrt-with-derivative2 [Double/TYPE x]
 (core/set-flag! :disp-final-source)
 (second
  (iterate-sqrt2 13 (variable x) (constant x))))


(defn expected-sqrt [x]
  {:x (Math/sqrt x)
   :dx (/ 0.5 (Math/sqrt x))})

(defn aeq [a b]
  (< (Math/abs (- a b)) 1.0e-6))

(defn almost-eq [a b]
  (and (aeq (:x a) (:x b))
       (aeq (:dx a) (:dx b))))


(deftest sqrt-test
  (is (aeq 3.34 3.340000003))
  (is (not (aeq 3.34 3.3403)))
  (is (map? (java/eval (sqrt-iteration (variable 2)
                                       (constant 3)))))
  (is (almost-eq (sqrt-with-derivative 2.0)
                 (expected-sqrt 2.0)))
  (is (almost-eq (sqrt-with-derivative2 2.0)
                 (expected-sqrt 2.0))))





