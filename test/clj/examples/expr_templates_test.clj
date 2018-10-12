(ns examples.expr-templates-test
  (:require [geex.lib :as l]
            [clojure.spec.alpha :as spec]
            [geex.java :as java]
            [geex.jcore :as core]
            [clojure.test :refer :all]))

;; "Expression templates" is a technique used in C++ to
;; implement efficient matrix operations:
;;
;; https://en.wikipedia.org/wiki/Expression_templates
;;
;; Here, we demonstrate this technique in Clojure and Geex.

(spec/def ::size any?)
(spec/def ::get fn?)
(spec/def ::expr (spec/keys :req-un [::size ::get]))

(def expr? (partial spec/valid? ::expr))



;;;------- Some common expression templates -------

(defn fill [n value]
  {:post [(expr? %)]}
  {:size n
   :get (fn [_] value)})

(defn array-expr [src-array]
  {:post [(expr? %)]}  
  {:size (l/count src-array)
   :get (fn [i] (l/aget src-array (l/cast Integer/TYPE i)))})

(defn range-expr [n]
  {:post [(expr? %)]}
  {:size n
   :get (fn [i] i)})

(defn add [a b]
  {:pre [(expr? a)
         (expr? b)]
   :post [(expr? %)]}
  {:size (:size a)
   :get (fn [i] (l/+ ((:get a) i)
                     ((:get b) i)))})

(defn mul [a b]
  {:pre [(expr? a)
         (expr? b)]
   :post [(expr? %)]}
  {:size (:size a)
   :get (fn [i] (l/* ((:get a) i)
                     ((:get b) i)))})

;; Note: This is not exactly the same thing
;; as (mul x x), because there we call the (:get ) function twice...
(defn sqr [x]
  {:size (:size x)
   :get (fn [i] (let [y ((:get x) i)]
                  (l/* y y)))})

(defn reverse-expr [x]
  {:pre [(expr? x)]
   :post [(expr? x)]}
  {:size (:size x)
   :get (fn [i]
          ((:get x) (l/- (:size x) i 1)))})

;;;------- Turns an expression into an array -------

(defn evaluate [expr]
  {:pre [(expr? expr)]}
  (let [n (:size expr)
        dst (l/make-array Double/TYPE n)
        g (:get expr)]
    (l/doseq [i (l/range (l/wrap 0) (l/wrap n))]
      (l/aset dst (l/cast Integer/TYPE i) (g i)))
    dst))

;;;------- Tests -------
(java/typed-defn add-to-array [(l/array-class Double/TYPE) arr
                               Double/TYPE offset]
                 ;(core/set-flag! :disp-time)
                 (evaluate (add (array-expr arr)
                                (fill (l/alength arr) offset))))



(defmacro eval-expr [& expr]
  `(vec (java/eval (evaluate ~@expr))))

(deftest various-tests
  (is (= (eval-expr (fill 3 119.0))
         [119.0 119.0 119.0]))
  (is (= (eval-expr (range-expr 3))
         [0.0 1.0 2.0]))
  (is (= (eval-expr (reverse-expr (range-expr 4)))
         [3.0 2.0 1.0 0.0]))
  (is (= (vec (add-to-array 
               (double-array [7 17 119])
               1000.0))
         [1007.0 1017.0 1119.0]))
  (is (= (eval-expr 
          (do
            ;(core/set-flag! :disp-final-source)
            (sqr (add (range-expr 9)
                      (fill 9 -4)))))
         [16.0 9.0 4.0 1.0 0.0 1.0 4.0 9.0 16.0])))
