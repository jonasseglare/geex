# lime

Is a Clojure macro library to generate inlined zero-overhead code from high-level Clojure expressions. In short, Lime code is Clojure code evaluated at macro expansion time (or whenever we choose to evaluate it). The result is a nested datastructure that encodes a trace of the job that the expression performed and how values flow in the program. From that datastructure, Clojure code can be generated, but potentially code for other platforms as well. More specifically, "Lime" stands for *Lightweight Inlined Meta Expressions*, that are

  * *Lightweight*: No custom syntax, no need to parse. Just plain Clojure code composed of functions and immutable data.

  * *Inlined*: Because the function calls are evaluated at macro expansion time, and their return values are *a dataflow trace of the job that the function performs*. So we could say that the function gets effectively inlined.

  * *Meta*: Expressions in lime are Clojure expressions that evaluate to nested datastructures that encode the flow of values in the code. From those datastructures programs can be generated. This means that we get a lot of control over how our code is generated, just like Clojure itself.

  * *Expressions*: Just Clojure expressions.

## What problem does Lime solve?

Lime is specifically designed for solving numerical problems common in optimization, engineering, computer vision, machine learning, etc. Problems where we work with matrices, minimize objective functions, simulate things, integrate things, and so on, where there is enough data to crunch for the computation time to be noticeable.

Popular languages in this domain include C++, Fortran, Matlab, Python, Julia, R, etc. 

## Usage

Try out the [Gorilla repl worksheet tutorial]() or [read the PDF]().

## Library structure

  * ```lime/core.clj```: Basic languages constructs (what used to be lang).
  * ```lime/seed.clj```: The most fundamental and common, 
                         platform-independent, concepts of lime.
  * ```lime/zest/*```: Platform specific code generation. In particular,
                       code for Clojure.
  * ```lime/juice/*```: A standard library with common functionalities, 
    inspired by the ```clojure.core``` library shipped with Clojure, 
    but also adding facilities for numerical computing such as 
    automatic differentiation, linear algebra, etc.

## License

Copyright © 2017 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
