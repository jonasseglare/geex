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

The square computation is factored out into its own function
