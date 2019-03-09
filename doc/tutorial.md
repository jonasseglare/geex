# Tutorial

We will start with a very small example to compute the square of a number. First, we need to include a few
namespaces.
```clj
(ns tutorial.core
  (:require [geex.core :as gx]
            [geex.java :as java]
            [geex.common :as c]))
```

In the above list, ```geex.core``` contains the core implementation of Geex for various platforms and core features such as control structures (e.g. ```If``` and ```Loop```).

The ```geex.java``` namespace provides features specific for the Java platform. The ```geex.common``` namespace contains common functions, such as mathematical operations ```+```, ```-```, etc.

The code below will generate an implementation for computing the square of a number:

```clj
(java/typed-defn square [Double/TYPE x]
                 (gx/set-flag! :disp :format :disp-time)
                 (c/* x x))
```
This results in a new function ```square``` that we can call, e.g.
```clj
(square 3.0)
;; => 9.0
```
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
```clj
(defn sqr [x]
  (c/* x x))

(java/typed-defn norm3 [[Double/TYPE Double/TYPE Double/TYPE] v]
                 (gx/set-flag! :disp :format)
                 (c/sqrt (apply c/+ (map sqr v))))
```

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
```clj
(def DoubleVec4 (vec (repeat 4 Double/TYPE)))
```
and then use that type to define a ```norm4``` function. We can also factor out the a actual computation into a general norm function (in case we wanted define a ```norm5``` function ;-) ):
```clj
(defn norm [v]
  (c/sqrt (apply c/+ (map sqr v))))

(java/typed-defn norm4 [DoubleVec4 v]
                 (norm v))

```
Let's test it, too:
```clj
(norm4 [1.0 2.0 3.0 4.0])
;; => 5.477225575051661
```
Great.

But what if we don't know the length of our vector? Maybe it is only known at *runtime*...

What if we pass a double-array?

Here is the code for that:
```clj
(java/typed-defn dynamic-norm [(c/array-class Double/TYPE) x]
                 (gx/set-flag! :disp :format)
                 (gx/Loop [squared-sum 0.0
                           counter 0]
                          (gx/If (c/< counter (c/count x))
                                 (gx/Recur (c/+ squared-sum (sqr (c/aget x counter)))
                                           (c/inc counter))
                                 (c/sqrt squared-sum))))
```
And we can test it on the classical example of the hypotenusa of a 3, 4, 5 triangle:
```clj
(dynamic-norm (double-array [3.0 4.0]))
;; => 5.0
```
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
```clj
(java/typed-defn transduce-dynamic-norm [(c/array-class Double/TYPE) x]
                 (c/sqrt
                  (c/transduce
                   (comp (c/map (partial c/aget x))
                         (c/map sqr))
                   c/+
                   0.0
                   (c/range (c/count x)))))
```
This will produce pretty much the same code as we saw before. And it works great:
```clj
(transduce-dynamic-norm (double-array [3.0 4.0]))
;; => 5.0
```

What about *complex* numbers?

We can represent the using maps:
```clj
(defn my-complex [re im]
  {:re re
   :im im})

(my-complex 3 4)
;; => {:re 3, :im 4}

```
What about performing operations on them?

We could write a function that lets us add complex number:
```clj
(defn add-complex [a b]
  {:re (c/+ (:re a) (:re b))
   :im (c/+ (:im a) (:im b))})


(add-complex (my-complex 3 4) (my-complex 2 8))
;; => {:re 5, :im 12}

```
But if we want to write *generic* code, we cannot use ```add-complex``` because it is too ```specific```. We would like to extend the ```c/+``` function. That can be done with *example-based multiple displatch. For that, we need to recognize our complex numbers:
```clj
(defn complex? [x]
  (and (map? x)
       (contains? x :re)
       (contains? x :im)))
```
and we also need to include the library that will provide us with the multiple dispatch mechanism:
```clj
(require '[bluebell.utils.ebmd :as ebmd])
```
Using the ```complex?``` function we just defined, we define a new argument type ```::complex```:
```clj
(ebmd/def-arg-spec ::complex {:pred complex?
                              :pos [(my-complex 3 4)]
                              :neg [0 9 :a {:kattskit 119}]})
```
At the pred key, we provide our predicate, that is ```complex?```.

The values at the ```:pos``` key are *positive* examples for which ```complex?``` returns *true*. The values at the ```:neg``` key are negative examples for which ```complex?``` returns *false*. This is needed in order to disambigutate between several matching predicates.

Anyway, now we can extend the ```c/+``` function for complex numbers:
```clj
(ebmd/def-poly c/binary-add [::complex a
                             ::complex b]
  {:re (c/+ (:re a) (:re b))
   :im (c/+ (:im a) (:im b))})
```
We could call ```c/binary-add``` directly:
```clj
(c/binary-add (my-complex 3 4) (my-complex 2 8))
;; => {:re 5, :im 12}
```
and it would work. But internally, the ```c/+``` function uses ```c/binary-add``` internally. The ```c/+``` function is also variadic, so we can use it with more arguments:
```clj
(c/+ (my-complex 3 4) (my-complex 2 8) (my-complex 1000 1000))
;; => {:re 1005, :im 1012}
```
This works great...

Until we attempt to add a real number to a complex number:
```clj
;; (c/+ (my-complex 3 4) 1000)
```
In which case we get a message like this:

```
   No matching call to polymorphic function geex.common/binary-add for
   these arguments 0: {:re 3, :im 4} 1: 1000
```

To fix this, we can automatically promote real numbers to complex numbers. To do that, we first include the namespace that specifies what we mean by a *real number*:
```clj
(require '[geex.ebmd.type :as gtype])
```
and we register the promotion:
```clj
(ebmd/register-promotion ::complex
                         (fn [real-number]
                           (my-complex real-number 0.0))
                         ::gtype/real)
```
This states that to convert an object matching the ```::gtype/real``` spec to a ```::complex``` number, construct that number by calling ```my-complex``` providing the real number at the real part.

Now suppose we are given a double-array of complex number stored contiguously in memory. We can now easily perform that computation with the ```c/+``` operator:
```clj
(java/typed-defn add-complex-numbers [(c/array-class Double/TYPE) numbers]
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

```
Let's check out a small test first:
```clj
(add-complex-numbers (double-array [3 4]))
;; => {:re 3.0, :im 4.0}
```
It seems to work.

What about two complex numbers?
```clj
(add-complex-numbers (double-array [3 4 4 9]))
;; => {:re 7.0, :im 13.0}
```
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