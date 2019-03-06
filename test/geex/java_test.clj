(ns geex.java-test
  (:import [geex.test EmptyInterface
            NumericInterface1
            MethodOverloading
            NumberToMapInterface
            StaticVarClass]
           [java.awt Point]
           [java.io File])
  (:require [clojure.test :refer :all]
            [geex.core.utils :refer [arity-partial]]
            [clojure.java.io :as io]
            [geex.java :refer :all :as java]
            [geex.core.seed :as seed]
            [geex.core :as core]
            [bluebell.utils.wip.debug :as debug]
            [clojure.spec.alpha :as spec]
            [geex.core.seedtype :as seedtype]
            [bluebell.utils.ebmd :as ebmd])
  (:refer-clojure :exclude [eval new]))

(def c (janino-cook-and-load-object
        "Kattskit"
        "public class Kattskit {public double sq(double x) {return x*x;}}"))


(deftest cooked-c-test
  (is (= 81.0 (.sq c 9))))

(deftest arglist-parse-test
  (is (= (#'java/parse-typed-defn-args '(kattskit [:cobra b :mjao d] (+ b d)))
         '{:name kattskit, :arglist [{:type :cobra, :name b}
                                     {:type :mjao, :name d}],
           :body [(+ b d)]})))

(typed-defn return-119-1
            [] 119.0)

(typed-defn return-119-2
            [(seed/typed-seed java.lang.Double/TYPE) x] 119.0)

(deftest return-119-test
    (is (= 119.0 (return-119-2 30))))

(typed-defn second-arg-fun2
            [(seed/typed-seed java.lang.Double/TYPE) x
             (seed/typed-seed java.lang.Long/TYPE) y
             (seed/typed-seed java.lang.Float/TYPE) z] y)

(deftest second-arg-test
    (is (= 119 (second-arg-fun2 3 119 4))))

(deftest is-a-test
    (is (isa? java.lang.Double java.lang.Number))
    (is (not (isa? java.lang.Number java.lang.Double))))

(typed-defn return-some-class2 [(seed/typed-seed java.lang.CharSequence) ch]
              ch)

(typed-defn check-cast2 [(seed/typed-seed java.lang.Object) obj]
              (#'java/unpack (seed/typed-seed java.lang.Double) obj))

(deftest return-some-class-test
    (is (= "kattskit" (return-some-class2 "kattskit")))
    (is (thrown? ClassCastException (return-some-class2 3)))
    (is (= 3.0 (check-cast2 3.0)))
    (is (thrown? ClassCastException (check-cast2 3))))

(deftest find-member-info-test
    (is (= 2 (count (#'java/find-member-info java.lang.String 'substring)))))

(typed-defn hash-code-test2 [(seed/typed-seed java.lang.String) obj]
              (call-method "hashCode" obj))

(deftest hash-code-test--
    (is (int? (hash-code-test2 "asdf"))))

(typed-defn substring-from2 [(seed/typed-seed java.lang.String) s
                             (seed/typed-seed java.lang.Integer/TYPE)
                             from]
              (call-method "substring" s from))

(deftest substring-2-test
    (is (= "cd" (substring-from2 "abcd" 2))))

(typed-defn int-to-float [(seed/typed-seed java.lang.Integer/TYPE) x]
            (call-method
             "floatValue"
             (call-static-method "valueOf" java.lang.Integer x)))

(deftest nested-calls-static-method-test
    (is (= 9.0 (int-to-float 9))))

(typed-defn box-float [seedtype/float x]
            (box x))

(typed-defn no-box-float [(seed/typed-seed java.lang.Float) x]
              (box x))

(deftest boxing-test
    (is (= 3.0 (box-float 3)))
    (is (= 3.0 (no-box-float (float 3.0)))))

(typed-defn unbox-float [(seed/typed-seed java.lang.Float) x]
              (unbox x))

(deftest unboxing-test
    (is (= 3.0 (unbox-float (float 3.0)))))

(typed-defn second-element-v [[seedtype/long seedtype/double] x]
              (let [[a b] x]
                b))

(deftest second-element-test
    (is (= 4.0 (second-element-v [3 4.0]))))

(typed-defn my-plus [seedtype/int a
                       seedtype/int b]
              (call-operator "+" a b))

(deftest my-plus-test
    (is (= 7 (my-plus 3 4))))

(typed-defn my-negate2 [seedtype/float x]
              (call-operator "-" x))

(deftest my-neg-test
    (is (= -9.0 (my-negate2 9))))

(typed-defn my-sq-norm [seedtype/int x
                        seedtype/int y]
            (call-operator "+"
                           (call-operator "*" x x)
                           (call-operator "*" y y)))

(deftest my-sq-norm-test
  (is (= 25 (my-sq-norm 3 4))))


(typed-defn double-square [seedtype/double a]
            (let [b (call-operator "+" a a)]
              (call-operator "*" b b)))

(deftest double-square-test
  (is (= 36.0 (double-square 3))))

  (typed-defn seqond2 [(list seedtype/int
                             seedtype/float
                             seedtype/double) x]
              (let [[a b c] x]
                (call-operator "+" a b c)))


  (deftest both-seq-unpacking-and-adding
    (is (= 12.0  (seqond2 (list (int 3) (float 4.0) 5.0)))))

  (typed-defn make-kwd2 [seedtype/string x]
              (call-static-method "intern"
                                  clojure.lang.Keyword
                                  x))

  (deftest keyword-test
    (is (= :asdf (make-kwd2 "asdf"))))

  (typed-defn make-magic-keyword []
              :kattskit)
(deftest kwyrod-test
  (is (= :kattskit (make-magic-keyword))))

(typed-defn make-magic-keyword2 []
            ::mu)

(deftest kwyrod-test2
    (is (= ::mu (make-magic-keyword2))))

(typed-defn add-a-b2 [{:a seedtype/long
                       :b seedtype/long} x]
            (call-operator "+" (:a x) (:b x)))

(deftest unpack-map-test
  (is (= 7 (add-a-b2 {:a 3 :b 4}))))

(typed-defn make-magic-symbol []
            'kattskit)

(deftest magic-sym-test
  (is (= 'kattskit (make-magic-symbol))))

(typed-defn make-magic-string []
            "Kattskit!")

(deftest string-test
  (is (= "Kattskit!"
         (make-magic-string))))

  (typed-defn eq-ints [seedtype/int a
                       seedtype/int b]
              (call-operator "==" a b))


  (typed-defn g-floats [seedtype/float a
                        seedtype/float b]
              (call-operator ">" a b))

  (typed-defn ne-chars [seedtype/char a
                        seedtype/char b]
              (call-operator "!=" a b))


  (deftest cmp-ops
    (is (eq-ints 119 119))
    (is (not (eq-ints 119 120)))
    (is (g-floats 3.4 3.0))
    (is (ne-chars \a \9))
    (is (not (ne-chars \a \a))))


(typed-defn implies [seedtype/boolean a
                     seedtype/boolean b]
            (call-operator "||" (call-operator "!" a) b))

(deftest ipmlies-test
  (is (implies false false))
  (is (implies false true))
  (is (not (implies true false)))
  (is (implies true true)))

(typed-defn dual-square2 [seedtype/double x]
            (let [y (call-operator "*" x x)]
              (call-operator "+" y y)))


(deftest ds2-test
  (is (= 162.0 (dual-square2 9))))

(typed-defn always-true2 []
            true)

(deftest true-test
  (is (always-true2)))

(typed-defn bit-and-f  [seedtype/long a
                        seedtype/int b]
            (call-operator "&" a b))

(deftest bit-and-test
  (= 2 (bit-and-f 6 3)))


(typed-defn make-seq [seedtype/int a
                      seedtype/double b]
            (list a b))

  (typed-defn make-vec [seedtype/int a
                        seedtype/float b
                        seedtype/double c]
              [a b c])

  (typed-defn make-map
              [seedtype/int a
               seedtype/int b]
              {:a a :b b})

  (deftest seq-test
    (is (= '(3 4.0)
           (make-seq 3 4)))
    (is (= [3 4.0 5.0]
           (make-vec 3 4 5)))
    (is (= {:a 3 :b 4}
           (make-map 3 4))))

(typed-defn comp-colls
            [seedtype/double a
             seedtype/double b
             {:a seedtype/long} c]
            {:ab (list a b)
             :c-vec [(list [c] c)]})


(deftest comp-colls-test
  (is (= (comp-colls 3 4 {:a 9})
         '{:c-vec [([{:a 9}] {:a 9})], :ab (3.0 4.0)})))


(typed-defn make-array-fn2 []
            (make-array-from-size java.lang.Integer/TYPE 9))

(typed-defn array-with-value [seedtype/int x]
            (let [dst (make-array-from-size java.lang.Integer/TYPE 1)]
              (set-array-element dst 0 x)
              dst))
(typed-defn complex-array-ops2 [seedtype/int a
                                seedtype/int b]
            (let [arr (make-array-from-size java.lang.Integer/TYPE 3)]
              (set-array-element arr 0 a)
              (set-array-element arr 1 b)
              (set-array-element arr 2
                                 (call-operator
                                  "+"
                                  (get-array-element arr 0)
                                  (get-array-element arr 1)))
              arr))


(typed-defn array-length-fn []
            (array-length (make-array-from-size java.lang.Integer 9)))

(deftest array-tests
  (let [arr (make-array-fn2)]
    (is (= 9 (count arr)))
    (is (= 119 (aget (array-with-value 119) 0)))
    (is (= [3 4 7] (vec (complex-array-ops2 3 4))))
    (is (= 9 (array-length-fn)))))

(typed-defn if-fun [seedtype/int x]
            (core/If (call-operator "<" x 9)
                     (core/to-seed 120)
                     (core/to-seed 119)))

(deftest if-test-with-fun
    (is (= 120 (if-fun 0)))
    (is (= 119 (if-fun 1000))))


(deftest call-method-args-test
    (is (= (spec/conform ::java/call-method-args [:pure "asdf" (class 1) 1 2 3])
           {:directives [:pure], :name "asdf", :dst java.lang.Long, :args [1 2 3]})))

(deftest import-type-signature-test
    (is (= {:a (core/typed-seed java.lang.Double)}
           (#'java/import-type-signature {:a  java.lang.Double}))))

(typed-defn compute-factorial3 [seedtype/long x]
            (core/fn-loop
             [x 1]
             (fn [[i product]]
               (core/If
                (call-operator "<=" i 0)
                product
                (core/Recur
                 (call-operator "-" i 1)
                 (call-operator "*" product i))))))

(deftest compute-factorial3-test
  (is (= (* 1 2 3 4 5)
         (compute-factorial3 5))))

(typed-defn compute-factorial4 [seedtype/long x]
            (core/Loop
             [i x
              product 1]
             (core/If (call-operator "<=" i 0)
                      product
                      (core/Recur
                       (call-operator "-" i 1)
                       (call-operator "*" product i)))))



(typed-defn small-anonymous-class-test []
            (instantiate
             {:super geex.test.EmptyInterface}))

(deftest anonymous-test
  (is (instance? geex.test.EmptyInterface
                 (small-anonymous-class-test))))

(typed-defn small-anonymous-class-test-2 []
            (instantiate
             {:super geex.test.EmptyInterface
              :variables [{:name "a"
                           :type Integer/TYPE}]}))
(deftest anonymous-test-2
  (is (instance? geex.test.EmptyInterface
                 (small-anonymous-class-test-2))))

(typed-defn small-anonymous-class-test-3 [Integer/TYPE x]
            (instantiate
             {:super geex.test.EmptyInterface
              :variables [{:name "a"
                           :type Integer/TYPE
                           :init x}]}))

(deftest anonymous-test-3
  (is (instance? geex.test.EmptyInterface
                 (small-anonymous-class-test-3 4))))

(typed-defn small-anonymous-class-test-4 [Integer/TYPE x]
            (let [y (call-operator "+"
                                   (core/wrap 1)
                                   x)]
              [(instantiate
                 {:super geex.test.EmptyInterface
                  :variables [{:name "a"
                               :type Integer/TYPE
                               :init y}]})
               y y y]))

(deftest anonymous-test-4
  (let [[a b c d] (small-anonymous-class-test-4 4)]
    (is (instance? geex.test.EmptyInterface a))
    (is (every? (partial = 5) [b c d]))))

(typed-defn small-anonymous-class-test-5 [{:b Double/TYPE} x]
            (instantiate
             {:super geex.test.EmptyInterface
              :variables [{:name "a"
                           :type {:b Double/TYPE}
                           :init x}]}))

(deftest anonymous-test-5
  (is (instance? geex.test.EmptyInterface
                 (small-anonymous-class-test-5 {:b 3.3}))))

(typed-defn ano-numeric-1 []
            (instantiate
             {:super geex.test.NumericInterface1
              :methods [{:name "apply"
                         :arg-types [Double/TYPE]
                         :fn (fn [this x]
                               (call-operator "*" 2.0 x))}]}))


(deftest numeric-ano-test-1
  (is (= 8.0 (.apply (ano-numeric-1) 4.0))))

(typed-defn ano-numeric-2 []
            (instantiate
             {:super geex.test.MapInterface
              :methods [{:name "apply"
                         :arg-types [{:a Double/TYPE}]
                         :fn (fn [this x]
                               (update x :a (partial
                                             call-operator
                                             "+" 1.0)))}
                        ]}))


(deftest numeric-ano-test-2
  (is (= {:a 4.0} (.apply (ano-numeric-2) {:a 3.0}))))

(typed-defn ano-numeric-3 []
            (instantiate
             {:super geex.test.MapInterface
              :variables [{:name "junkvariable"
                           :type Double/TYPE}]
              :methods [{:name "apply"
                         :arg-types [{:a Double/TYPE}]
                         :fn (fn [this x]
                               (update x :a (partial
                                             call-operator
                                             "+" 1.0)))}
                        {:name "junkMethod"
                         :arg-types [Double/TYPE]
                         :fn (fn [this x]
                               (call-operator "*" x x x))}
                        ]}))

(deftest numeric-ano-test-3
  (is (= {:a 4.0} (.apply (ano-numeric-3) {:a 3.0}))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Classes
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(deftest class-rendering
  (is (sequential?
       (:result (render-class-data {:name "Mjao"})))))

(def mul-by-1000
  (make-class {:name "Ko"
               :methods [{:name "apply"
                          :arg-types [Double/TYPE]
                          :fn (fn [this x]
                                (call-operator
                                 "*"
                                 1000.0 x))}]}))

(deftest mul-by-1000-test
  (is (= (.apply (.newInstance mul-by-1000) 119.0)
         119000.0)))

(deftest eval-test
  (is (= 7 (eval (call-operator "+" 3 4)))))


(deftest method-overloading
  (is (= 4 (eval (call-static-method
                  "add" MethodOverloading
                  (core/wrap 1) (core/wrap 3)))))
  (is (= "ab"
         (eval (call-static-method
                "add" MethodOverloading
                (core/wrap "a") (core/wrap "b"))))))

(deftest stub-class-test
  (let [cl (make-stub-class 
            {:name "Mjao"
             :methods
             [{:name "add"
               :arg-types [Integer/TYPE Float/TYPE]
               :ret Integer/TYPE
               :fn identity}]
             :variables 
             [{:name "a"
               :type Integer/TYPE}
              {:name "b"
               :static? true
               :visibility :private
               :type {:kattskit Integer/TYPE}}]}
            0
            false)
        instance (.newInstance cl)]
    (is (class? cl))
    (is (= 0 (.a instance)))
    (is (= 0 (.add instance 3 2.0)))
    (is (stub-class? cl))
    (is (= "Mjao" (typename cl)))
    (is (not (stub-class? Integer))))
  (let [cl (make-stub-class 
            {:name ""}
            0
            false)]
    (is (thrown? Exception (typename cl))))
  (let [cl (make-stub-class 
            {:name ""
             :package "kattskit"}
            0
            false)]
    (is (thrown? Exception (typename cl)))))

(deftest stub-class-name-tag
  (is (= 1 1)))



;; Calling method on anonymous classes is not supported
(typed-defn recursive-factorial-1 []
            (instantiate
             {:super geex.test.NumericInterface1
              :methods
              [{:name "apply"
                :arg-types [Double/TYPE]
                :ret Double/TYPE
                :fn (fn [this x]
                      (assert (nil? this))
                      (core/If
                       (call-operator "<=" x 0.0)
                       1.0
                       (call-operator
                        "*"
                        x (call-method
                           "apply" (this-object)
                           (call-operator "-" x 1.0)))))}]}))

(deftest rec-fac-test
  (is (= 24.0 (.apply (recursive-factorial-1) 4.0))))

(typed-defn recursive-factorial-2 []
            (instantiate
             {:super geex.test.NumericInterface1
              :methods
              [{:name "apply"
                :arg-types [Double/TYPE]
                :ret Double/TYPE
                :fn (fn [this x]
                      (nil? this)
                      (call-static-method
                       "staticApply"
                       (this-class)
                       x))}
               {:name "staticApply"
                :arg-types [Double/TYPE]
                :ret Double/TYPE
                :static? true
                :fn (fn [this-type x]
                      (nil? this-type)
                      (call-operator "+" 119.0 x))}]}))

(deftest rec-fac-2-test
  (is (= 122.0 (.apply (recursive-factorial-2) 3))))



(def wrapped-factorial-result {:result Double/TYPE})

(typed-defn recursive-factorial-3 []
            (instantiate
             {:super geex.test.NumberToMapInterface
              :methods
              [{:name "apply"
                :arg-types [Double/TYPE]
                :ret wrapped-factorial-result
                :fn (fn [this x]
                      (assert (nil? this))
                      {:result
                       (core/If
                        (call-operator "<=" x 0.0)
                        1.0
                        (call-operator
                         "*"
                         x (:result
                            (unpack
                             wrapped-factorial-result
                             (call-method
                              "apply" (this-object)
                              (call-operator "-" x 1.0))))))})}]}))

(deftest recursive-factorial-test-with-map-result
  (is (= (.apply (recursive-factorial-3) 4)
         {:result 24.0})))


(typed-defn
 local-class-fn [Double/TYPE x]
 (with-local-class
   {:name "Mjao"
    :methods [{:name "factorial"
               :static? true
               :ret Double/TYPE
               :arg-types [Double/TYPE]
               :fn (fn [this x]
                     (core/If
                      (call-operator "<=" x 0.0)
                      1.0
                      (call-operator
                       "*"
                       x (call-static-method
                          "factorial"
                          this
                          (call-operator "-" x 1.0)))))}]}
   (fn [cl]
     (call-static-method "factorial" cl x))))

(deftest local-class-test
  (is (= 120.0 (local-class-fn 5.0))))

;; Local interface in a method body is not supported
#_(typed-defn
 local-interface-test [Double/TYPE x]
 (core/set-flag! :disp)
 (with-local-class
   {:name "NumberGen"
    :interface? true
    :methods [{:name "getIt"
               :ret Double/TYPE
               :arg-types []}]}
   (fn [cl]
     9)))

(def constructable-class
  (make-class
   {:name "Mjao"
    :constructors
    [{:fn (fn [this x])
      :arg-types [Integer/TYPE]}]
    :flags [
            ;;:disp
            ]}))

(deftest constructor-test
  (let [cl constructable-class]
    (is (class? cl))))

#_(def I (definterface MyInterface
         (^int method1 [^int x])))

(typed-defn
 stub-constructor-fn []
 ;(core/set-flag! :disp)
 (with-local-class {:name "Mjao"
                    :constructors
                    [{:arg-types [Double/TYPE]
                      :fn (fn [this x])}]
                    
                    #_:methods
                    #_[{:ret Integer/TYPE
                      :name "method1"
                      :arg-types [Integer/TYPE]
                      :fn (fn [_ x]
                            (int 119))}]}
   (fn [cl]
     (class? cl)
     9)))

(deftest stub-constructor-test
  (is (= 9 (stub-constructor-fn))))


(typed-defn let-class-fn []
            (let-class [a {:name "A"
                           :methods
                           [{:static? true
                             :name "get"
                             :arg-types []
                             :fn (fn [this] 7)
                             :ret Long/TYPE}]}
                        b {:name "B"
                           :methods
                           [{:static? true
                             :name "get"
                             :arg-types []
                             :fn (fn [this] 17)
                             :ret Long/TYPE
                             }]}]
              (assert (class? a))
              (assert (class? b))
              (call-operator
               "*"
               (call-static-method "get" a)
               (call-static-method "get" b))))

(deftest let-class-test
  (is (= 119 (let-class-fn))))

(typed-defn
 new-fn-0 []
 (java/new java.lang.Integer (int 9)))

(deftest call-constructor-test
  (is (= 9 (new-fn-0))))

(def instance-var-class
  (make-class {:name "Mjao"
               :variables [{:name "a"
                            :type Integer/TYPE
                            :init 3}]
               :constructors
               [{:arg-types []
                 :fn (fn [this]
                       (set-instance-var this "a" 119))}]}))

(deftest set-instance-var-test
  (is (= 119 (.a (.newInstance instance-var-class)))))

(typed-defn make-a-point []
            (let [pt (java/new Point)]
              (set-instance-var pt "x" 3)
              (set-instance-var pt "y" 4)
              pt))

(deftest make-a-point-test
  (let [pt (make-a-point)]
    (is (= 3 (.x pt)))
    (is (= 4 (.y pt)))))

(typed-defn point-to-vec [Point pt]
            [(get-instance-var "x" pt)
             (get-instance-var "y" pt)])

(deftest point-to-vec-test
  (is (= [3 4] (point-to-vec (Point. 3 4)))))

(typed-defn get-instance-var-sum []
            (let-class [a {:name "X"
                           :variables
                           [{:name "a"
                             :type Integer/TYPE
                             :init 3}
                            {:name "b"
                             :type Integer/TYPE
                             :init 4}]
                           :methods
                           [{:name "sum"
                             :ret Integer/TYPE
                             :arg-types []
                             :fn (fn [this]
                                   (call-operator
                                    "+"
                                    (get-instance-var "a" this)
                                    (get-instance-var "b" this)))}]}]
              (let [x (java/new a)]
                (call-method "sum" x))))

(deftest instance-var-sum-test
  (is (= 7 (get-instance-var-sum))))

(typed-defn modify-static-var [Integer/TYPE x]
            ;(core/set-flag! :disp)
            (set-static-var "A" StaticVarClass x))

(deftest set-static-var-test
  (modify-static-var 119)
  (is (= StaticVarClass/A 119))
  (modify-static-var 120)
  (is (= StaticVarClass/A 120)))

(typed-defn read-static-var []
            (get-static-var "A" StaticVarClass))

(deftest read-static-var-test
  (modify-static-var 130)
  (is (= 130 (read-static-var))))


(typed-defn throws-something []
            (java/throw
             (java/new RuntimeException "Mjao")))

(deftest throw-test
  (is (thrown? RuntimeException
               (throws-something)))
  (try
    (throws-something)
    (is false)
    (catch RuntimeException e
      (is (= (.getMessage e)
             "Mjao")))))


(deftest settings-test
  (spec/valid? ::java/settings {:java-source-path "abc"
                                :package-from-namespace? true})
  (spec/valid? ::java/settings {:java-source-path (File.  "abc")
                                :package-from-namespace? false}))

(deftest write-file-test
  (let [class-def {:name "Mjao"
                   :package "geex.test"}
        settings {:java-source-path "src/java"
                  :package-from-namespace? false}
        full-path "src/java/geex/test/Mjao.java"
        file (io/file full-path)]
    (if (.exists file)
      (io/delete-file file))
    (is (not (.exists file)))
    (write-source-files [[[[class-def]]]] settings)
    (is (.exists file))))


(java/typed-defn refer-to-loop-var []
                 (core/Loop [i 1
                             product 1.0]
                            (core/If (call-operator "<=" i 4)
                                     (let [my-obj
                                           (instantiate
                                            {:super NumericInterface1
                                             :methods
                                             [{:arg-types [Double/TYPE]
                                               :name "apply"
                                               :fn (fn [_ prod]
                                                     (call-operator "*" i prod))}]})]
                                       (core/Recur (call-operator "+" i 1)
                                                   (call-method "apply" my-obj product)))
                                     product)))


(deftest inner-class-loop-test
  (is (= 24.0 (refer-to-loop-var))))

;; PROBLEMATIC: Calling a method from another one.
(defn make-c []
  (.newInstance
   (make-class 
    {:name "Cl"
     :methods [{:name "a"
                :arg-types [Integer/TYPE]
                :ret Long/TYPE
                :fn (fn [this x] 119)}
               {:name "b"
                :arg-types [Integer/TYPE]
                :fn (fn [this x]
                      (call-method 
                       "a" 
                       (this-object)
                       x))}]})))

(deftest method-call-test
  (is (= 119 (.b (make-c) 0))))



(typed-defn char-to-int [Character/TYPE c]
            (switch
             c
             \a 0
             \b 1
             119))

(deftest switch-test
  (is (= 0 (char-to-int \a)))
  (is (= 1 (char-to-int \b)))
  (is (= 119 (char-to-int \c))))


(typed-defn char-to-int2 [Character/TYPE c]
            (switch-fn
             c
             [[\a (constantly 0)]
              [\b (constantly 1)]]
             (constantly 119)))

(deftest switch-test2
  (is (= 0 (char-to-int2 \a)))
  (is (= 1 (char-to-int2 \b)))
  (is (= 119 (char-to-int2 \c))))

(def add-3 (arity-partial + 1 4 #{3}))

(deftest arity-partial-test
  (is (= 9 (add-3 4)))
  (is (thrown? Exception (add-3 4 5))))
