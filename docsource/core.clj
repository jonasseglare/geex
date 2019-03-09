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
(java/typed-defn dynamic-norm [(c/array-class Double/TYPE) x]
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

"
