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
                 (c/* x x))
"
This results in a new function ```square``` that we can call, e.g.
"
(square 3.0)
;; => 9.0
"

"
