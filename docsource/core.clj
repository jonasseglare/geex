"
# Tutorial

We will start with a very small example to compute the square of a number. First, we need to include a few
namespaces.
"
(ns tutorial.core
  (:require [geex.core :as gx]
            [geex.java :as java]
            [geex.common :as c]))
"

In the above list, ```geex.core``` contains the core implementation of Geex for various platforms and core features such as control structures (e.g. ```If``` and ```Loop```).

The ```geex.java``` namespace provides features specific for the Java platform. The ```geex.common``` namespace contains common functions, such as mathematical operations ```+```, ```-```, etc.

The code below will generate an implementation for computing the square of a number:

"
(java/typed-defn square [Double/TYPE x]
                 (gx/set-flag! :disp :format :disp-time)
                 (c/* x x))
"
This results in a new function ```square``` that we can call, e.g.
"
(square 3.0)
;; => 9.0
"
The top-line ```[Double/TYPE x]``` specifies that this function takes a parameter of type ```Double/TYPE``` that we call ```x```.

The line ```(c/* x x)``` performs the actual squaring. Here we use the ```*``` function from the ```geex.common``` namespace instead of ```clojure.core/*```.

The line ```(gx/set-flag! :disp :format :disp-time)``` tells the code generator to output extra information:

  * ```:disp``` means \"display source code\"
  * ```:format``` means \"format the source code\"
  * ```:disp-time``` means \"display a log with times\"

The generates source code that gets displayed looks like this.
```java
package tutorial_pcore;

public class TypedDefn__square {
  /* Various definitions */
  public double apply(final double arg00) {
    return (arg00 * arg00);
  }
}
```

If we were to remove the ```:format``` key, we would instead get 
```java
 package  tutorial_pcore ;   public  class TypedDefn__square   { /* Various definitions */      public double apply (  final double arg00 ) {     return   (  arg00  * arg00 ) ; }  }
```

The time for generating this code is displayed in a time report:
```
--- Time report ---
Start: 0.00
Evaluated state: 0.00300
Generated code: 0.00400
Composed class: 0.00400
Formatted code: 0.0100
Created compiler: 0.0100
Compiled it: 0.0120
Loaded class: 0.0120

Number of seeds: 17
Time per seed: 7.05873264985926E-4
```
We see that formatting the code takes a small amount of the time, so removing the ```:format``` key can improve the time. But typically, generating the code is very fast so it hardly matters.

What about a more complex example? Maybe computing the norm of a 3d vector?

This is what the code for doing that looks like:
"
(defn sqr [x]
  (c/* x x))

(java/typed-defn norm3 [[Double/TYPE Double/TYPE Double/TYPE] v]
                 (gx/set-flag! :disp :format)
                 (c/sqrt (apply c/+ (map sqr v))))
"

The square computation is factored out into its own function. 

The code is generated using ```java/typed-defn```, just like in the previous example. The input type here is an immutable vector where every type is a Double/TYPE.

Computing the norm is done using a combination of Clojure and Geex functions: We use ```map``` and ```apply``` to give structure to the computation, but the actual computations are done with ```c/sqrt```, ```c/*``` and ```c/+```. It results in this implementation:

```java
package tutorial_pcore;

public class TypedDefn__norm3 {
  /* Various definitions */
  public double apply(final clojure.lang.IPersistentVector arg00) {
    final java.lang.Object s0007 = (arg00.nth(0));
    final double s0009 = (((java.lang.Double) s0007).doubleValue());
    final java.lang.Object s0011 = (arg00.nth(1));
    final double s0013 = (((java.lang.Double) s0011).doubleValue());
    final java.lang.Object s0015 = (arg00.nth(2));
    final double s0017 = (((java.lang.Double) s0015).doubleValue());
    return (java.lang.Math.sqrt((((s0009 * s0009) + (s0013 * s0013)) + (s0017 * s0017))));
  }
}
```

Suppose we wanted to compute the norm of a 4-dimensional vector instead. That long argument list is not very nice. We can generate it and give it a name:
"
(def DoubleVec4 (vec (repeat 4 Double/TYPE)))
"
and then use that type to define a ```norm4``` function. We can also factor out the a actual computation into a general norm function (in case we wanted define a ```norm5``` function ;-) ):
"
(defn norm [v]
  (c/sqrt (apply c/+ (map sqr v))))

(java/typed-defn norm4 [DoubleVec4 v]
                 (norm v))

" 
Let's test it, too:
"
(norm4 [1.0 2.0 3.0 4.0])
;; => 5.477225575051661
"
Great.

But what if we don't know the length of our vector? Maybe it is only known at *runtime*...

What if we pass a double-array?

Here is the code for that:
"
(java/typed-defn dynamic-norm [(c/array-type Double/TYPE) x]
                 (gx/set-flag! :disp :format)
                 (gx/Loop [squared-sum 0.0
                           counter 0]
                          (gx/If (c/< counter (c/count x))
                                 (gx/Recur (c/+ squared-sum (sqr (c/aget x counter)))
                                           (c/inc counter))
                                 (c/sqrt squared-sum))))
"
And we can test it on the classical example of the hypotenusa of a 3, 4, 5 triangle:
"
(dynamic-norm (double-array [3.0 4.0]))
;; => 5.0
"
It works as we expect.

In this example, we just replace the classical control structures of Clojure by the Geex siblings:

  * Replace ```if``` by ```gx/If```
  * Replace ```loop``` by ```gx/Loop```
  * Replace ```recur``` by ```gx/Recur```

This generates the following code:

```java 
package tutorial_pcore;

public class TypedDefn__dynamic_dnorm {
  /* Various definitions */
  public double apply(final double[] arg00) {
    double lvar0 = 0.0;
    long lvar1 = 0;
    double lvar2 = 0.0;
    double lvar3 = 0.0;
    lvar0 = 0.0;
    lvar1 = 0L;
    while (true) {
      final double s0014 = lvar0;
      final long s0015 = lvar1;
      if ((s0015 < (arg00.length))) {
        final double s0021 = (arg00[((int) s0015)]);
        lvar0 = (s0014 + (s0021 * s0021));
        lvar1 = (s0015 + 1L);
        continue;
      } else {
        lvar2 = (java.lang.Math.sqrt(s0014));
      }
      final double s0039 = lvar2;
      lvar3 = s0039;
      break;
    }
    final double s0045 = lvar3;
    return s0045;
  }
}
```

Of course, we can still use the true ```if```, ```loop``` and ```recur``` form in our code during code generation time.

But looping like this is not very elegant, is it? Can't we use transducers for this? Yes we can!
"
(java/typed-defn transduce-dynamic-norm [(c/array-type Double/TYPE) x]
                 (c/sqrt
                  (c/transduce
                   (comp (c/map (partial c/aget x))
                         (c/map sqr))
                   c/+
                   0.0
                   (c/range (c/count x)))))
"
This will produce pretty much the same code as we saw before. And it works great:
"
(transduce-dynamic-norm (double-array [3.0 4.0]))
;; => 5.0
"

What about *complex* numbers?

We can represent the using maps:
"
(defn my-complex [re im]
  {:re re
   :im im})

(my-complex 3 4)
;; => {:re 3, :im 4}

"
What about performing operations on them?

We could write a function that lets us add complex number:
"
(defn add-complex [a b]
  {:re (c/+ (:re a) (:re b))
   :im (c/+ (:im a) (:im b))})


(add-complex (my-complex 3 4) (my-complex 2 8))
;; => {:re 5, :im 12}

"
But if we want to write *generic* code, we cannot use ```add-complex``` because it is too ```specific```. We would like to extend the ```c/+``` function. That can be done with *example-based multiple displatch. For that, we need to recognize our complex numbers:
"
(defn complex? [x]
  (and (map? x)
       (contains? x :re)
       (contains? x :im)))
"
and we also need to include the library that will provide us with the multiple dispatch mechanism:
"
(require '[bluebell.utils.ebmd :as ebmd])
"
Using the ```complex?``` function we just defined, we define a new argument type ```::complex```:
"
(ebmd/def-arg-spec ::complex {:pred complex?
                              :pos [(my-complex 3 4)]
                              :neg [0 9 :a {:kattskit 119}]})
"
At the pred key, we provide our predicate, that is ```complex?```.

The values at the ```:pos``` key are *positive* examples for which ```complex?``` returns *true*. The values at the ```:neg``` key are negative examples for which ```complex?``` returns *false*. This is needed in order to disambigutate between several matching predicates.

Anyway, now we can extend the ```c/+``` function for complex numbers:
"
(ebmd/def-poly c/binary-add [::complex a
                             ::complex b]
  {:re (c/+ (:re a) (:re b))
   :im (c/+ (:im a) (:im b))})
"
We could call ```c/binary-add``` directly:
"
(c/binary-add (my-complex 3 4) (my-complex 2 8))
;; => {:re 5, :im 12}
"
and it would work. But internally, the ```c/+``` function uses ```c/binary-add``` internally. The ```c/+``` function is also variadic, so we can use it with more arguments:
"
(c/+ (my-complex 3 4) (my-complex 2 8) (my-complex 1000 1000))
;; => {:re 1005, :im 1012}
"
This works great...

Until we attempt to add a real number to a complex number:
"
;; (c/+ (my-complex 3 4) 1000)
"
In which case we get a message like this:

```
   No matching call to polymorphic function geex.common/binary-add for
   these arguments 0: {:re 3, :im 4} 1: 1000
```

To fix this, we can automatically promote real numbers to complex numbers. To do that, we first include the namespace that specifies what we mean by a *real number*:
"
(require '[geex.ebmd.type :as gtype])
"
and we register the promotion:
"
(ebmd/register-promotion ::complex
                         (fn [real-number]
                           (my-complex real-number 0.0))
                         ::gtype/real)
"
This states that to convert an object matching the ```::gtype/real``` spec to a ```::complex``` number, construct that number by calling ```my-complex``` providing the real number at the real part.

Now suppose we are given a double-array of complex number stored contiguously in memory. We can now easily perform that computation with the ```c/+``` operator:
"
(java/typed-defn add-complex-numbers [(c/array-type Double/TYPE) numbers]
                 (let [n (c/quot (c/cast Long/TYPE (c/count numbers)) 2)]
                   (c/transduce

                    ;; Map an index in the array to a complex number
                    ;; extracted from that array.
                    (c/map (fn [index]
                             (let [at (c/* 2 index)]
                               (my-complex (c/aget numbers (c/+ at 0))
                                           (c/aget numbers (c/+ at 1))))))
                    
                    c/+ ;; With *our* overload.
                    (my-complex 0.0 0.0)
                    (c/range n))))

"
Let's check out a small test first:
"
(add-complex-numbers (double-array [3 4]))
;; => {:re 3.0, :im 4.0}
"
It seems to work.

What about two complex numbers?
"
(add-complex-numbers (double-array [3 4 4 9]))
;; => {:re 7.0, :im 13.0}
"
Works, too. What about the code?

```java
package tutorial_pcore;

public class TypedDefn__add_dcomplex_dnumbers {
  /* Various definitions */
  static clojure.lang.Keyword INTERNED__Keyword___cim = clojure.lang.Keyword.intern(\"im\");
  static clojure.lang.Keyword INTERNED__Keyword___cre = clojure.lang.Keyword.intern(\"re\");

  public clojure.lang.IPersistentMap apply(final double[] arg00) {
    double lvar0 = 0.0;
    double lvar1 = 0.0;
    long lvar2 = 0;
    long lvar3 = 0;
    long lvar4 = 0;
    double lvar5 = 0.0;
    double lvar6 = 0.0;
    double lvar7 = 0.0;
    double lvar8 = 0.0;
    final long s0010 = (clojure.lang.Numbers.quotient(((long) (arg00.length)), 2L));
    lvar0 = 0.0;
    lvar1 = 0.0;
    lvar2 = 0L;
    lvar3 = ((s0010 - 0L) / 1L);
    lvar4 = 1L;
    while (true) {
      final double s0025 = lvar0;
      final double s0026 = lvar1;
      final long s0027 = lvar2;
      final long s0028 = lvar3;
      final long s0029 = lvar4;
      if ((s0028 <= 0L)) {
        lvar5 = s0025;
        lvar6 = s0026;
      } else {
        final 
long s0041 = (2L * s0027);
        final double s0045 = (arg00[((int) (s0041 + 0L))]);
        final double s0049 = (arg00[((int) (s0041 + 1L))]);
        lvar0 = (s0025 + s0049);
        lvar1 = (s0026 + s0045);
        lvar2 = (s0027 + s0029);
        lvar3 = (s0028 - 1L);
        lvar4 = s0029;
        continue;
      }
      final double s0065 = lvar5;
      final double s0066 = lvar6;
      lvar7 = s0065;
      lvar8 = s0066;
      break;
    }
    final double s0073 = lvar7;
    final double s0074 = lvar8;
    return clojure.lang.PersistentHashMap.create(
        (java.lang.Object) (INTERNED__Keyword___cim),
        (java.lang.Object) (s0073),
        (java.lang.Object) (INTERNED__Keyword___cre),
        (java.lang.Object) (s0074));
  }
}
```
Despite making use of maps, dynamic dispatch, etc, we end up with quite flat code. This is what gives us the performance: All the unnecessary stuff that we use to express our computations is shaved away.

## Types

Even if Clojure is dynamically typed, the code generation process propagates and requires static type information. The types used are those of the host platform, e.g. the JVM, as well as compositions of those types using immutable data structures.

Most of the time, the type information is propagated naturally and we don't have to specify types except for in the argument lists of functions. Let's look a bit at how types work. We create a dummy function that takes an argument in order to study how it works.

"
(require '[geex.core.seed :as seed])

(java/typed-defn just-for-the-sake-of-looking-at-types [Double/TYPE A]
                 (println "A is" A)
                 (println "The type of A is" (gx/type-signature A))
                 (println "The underlying runtime type of A is"
                          (-> A
                              gx/type-signature
                              seed/datatype)))
"
This code will, when we load the function, print out
```
A is #object[geex.DynamicSeed 0x6a67c7f8 ISeed(type=double, id=5, desc=bind-name)]
The type of A is #object[geex.TypedSeed 0x497eff08 ISeed(type=double, desc=TypedSeed)]
The underlying runtime type of A is double
```

The first row tells us that a is some object of type DynamicSeed. Seeds are used to track the computations that we perform. Then next row specifies the type A during *code generation time*. The final row is the type of A *during runtime*, which is ```double``` as we would expect.

If, instead, we want our function to take an array of doubles, we can use ```c/array-type``` to construct an array type.
"
(java/typed-defn this-function-takes-an-array [(c/array-type Double/TYPE) B]
                 (println "B is" B)
                 (println "The type of B is" (gx/type-signature B))
                 (println "The underlying runtime type of B is"
                          (-> B
                              gx/type-signature
                              seed/datatype)))
"
which prints out
```
B is #object[geex.DynamicSeed 0x34c0c2df ISeed(type=class [D, id=5, desc=bind-name)]
The type of B is #object[geex.TypedSeed 0x420a013b ISeed(type=class [D, desc=TypedSeed)]
The underlying runtime type of B is [D
```

## Sequences

Maybe the easiest sequences to construct is a range. Suppose we want to compute the factorial. We can do that with the help of sequences:
"
(java/typed-defn factorial [Long/TYPE n]
                 (c/reduce c/* 1.0 (c/rest (c/range (c/inc n)))))

(factorial 4)
;; => 24.0

"
Common sequence functions, such as ```reduce```, ```rest```, ```range``` are implemented in the ```geex.common``` namespace.

Mapping also works. So for instance, we can compute a dot product like this:
"
(java/typed-defn dot-product [(c/array-type Double/TYPE) a
                              (c/array-type Double/TYPE) b]
                 (c/reduce
                  c/+
                  0.0
                  (c/map c/* a b)))
"
and call it like this:
"
(dot-product (double-array [3 4 5])
             (double-array [9 2 2]));; => 45.0
"
There are also functions to test if the elements in a collection satisfy some predicate.

We can use ```some``` and ```every?``` for that:
"
(java/typed-defn near? [(c/array-type Double/TYPE) a
                        (c/array-type Double/TYPE) b
                        Double/TYPE tol]
                 
                 (let [near-scalar?
                       (fn [[a b]] (c/< (c/abs (c/- a b)) tol))]

                   (gx/set-flag! :disp :format)
                   
                   (c/every? near-scalar? (c/map vector a b))))

(near? (double-array [1 2 3])
                      (double-array [1.001 1.9999 3.000324])
                      0.01);; => true

(near? (double-array [1 2 3])
                      (double-array [1.001 1.9999 3.000324])
                      0.000001)
;; => false

"
Again, this results in a long, but nevertheless quite lean implementation (without lazy sequences, etc), that the JIT can probably optimize even further:

```java
package tutorial_pcore;

public class TypedDefn__near_q {
  /* Various definitions */
  public boolean apply(final double[] arg00, final double[] arg01, final double arg02) {
    double[] lvar0 = null;
    int lvar1 = 0;
    int lvar2 = 0;
    double[] lvar3 = null;
    int lvar4 = 0;
    int lvar5 = 0;
    boolean lvar6 = false;
    boolean lvar7 = false;
    boolean lvar8 = false;
    boolean lvar9 = false;
    double lvar10 = 0.0;
    boolean lvar11 = false;
    boolean lvar12 = false;
    lvar0 = arg00;
    lvar1 = 0;
    lvar2 = (arg00.length);
    lvar3 = arg01;
    lvar4 = 0;
    lvar5 = (arg01.length);
    while (true) {
      final double[] s0022 = lvar0;
      final int s0023 = lvar1;
      final int s0024 = lvar2;
      final double[] s0025 = lvar3;
      final int s0026 = lvar4;
      final int s0027 = lvar5;
      if ((0L == s0024)) {
        lvar6 = true;
      } else {
        if ((0L == s0027)) {
          lvar7 = true;
        } else {
          lvar7 = false;
        }
        final boolean 
s0054 = lvar7;
        if (s0054) {
          lvar8 = true;
        } else {
          lvar8 = false;
        }
        final boolean s0069 = lvar8;
        lvar6 = s0069;
      }
      final boolean s0075 = lvar6;
      if (s0075) {
        lvar9 = true;
      } else {
        final double s0084 = (s0022[s0023]);
        final double s0085 = (s0025[s0026]);
        final double s0086 = (s0084 - s0085);
        if ((s0086 < 0.0)) {
          lvar10 = (-s0086);
        } else {
          lvar10 = s0086;
        }
        final double s0102 = lvar10;
        if ((s0102 < arg02)) {
          lvar0 = s0022;
          lvar1 = ((int) (s0023 + 1L));
          lvar2 = ((int) (s0024 - 1L));
          lvar3 = s0025;
          lvar4 = ((int) (s0026 + 1L));
          lvar5 = ((int) (s0027 - 1L));
          continue;
        } else {
          lvar11 = false;
        }
        final boolean s0135 = lvar11;
        lvar9 = s0135;
      }
      final boolean s0141 = lvar9;
      lvar12 = s0141;
      break;
    }
    final 
boolean s0147 = lvar12;
    return s0147;
  }
}
```
"
