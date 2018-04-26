# lime

Polhem* is a library for writing simple, composable and reusable code that generates high-performance low level code, suitable for applications in optimization, image processing, machine learning, computer vision, graphics, games, statistics and many other domains.

It achieves this using a statically typed value oriented approach where every part of the generated program is expressed using immutable data structures. These values can be manipulated however we like using all of Clojure, giving us both metaprogramming powers and simplicity.

The objective of Polhem is to combine the productivity, simplicity and reach of Clojure with improved performance for computationally intense applications.

* Performance Oriented Library for Highly Expressive Metaprogramming

## Names

POLHEM: Performance Oriented Library for Highly Expressive Metaprogramming

Flatrod: The standard library.

##

Is a Clojure macro library to generate inlined zero-overhead code from high-level Clojure expressions. In short, Lime code is Clojure code evaluated at macro expansion time (or whenever we choose to evaluate it). The result is a nested datastructure that encodes a trace of the job that the expression performed and how values flow in the program. From that datastructure, Clojure code can be generated, but potentially code for other platforms as well. More specifically, "Lime" stands for *Lightweight Inlined Meta Expressions*. Lime expressions are

  * *Lightweight*: No custom syntax, no need to parse. Implemented as yet another library. Just plain Clojure code composed of functions and immutable data. Minimal glue code to interact with the rest of the Clojure code.

  * *Inlined*: Because the function calls are evaluated at macro expansion time, and their return values are *a dataflow trace of the job that the function performs*. So we could say that the function gets effectively inlined.

  * *Meta*: Expressions in lime are Clojure expressions that evaluate to nested datastructures that encode the flow of values in the code. From those datastructures programs can be generated. This means that we get a lot of control over how our code is generated and can run any code we like during "compile time".

## What problem does Lime solve?

Lime is specifically designed for solving numerical problems common in optimization, engineering, computer vision, machine learning, etc. Problems where we work with matrices, minimize objective functions, simulate things, integrate things, and so on, where there is enough data to crunch for the computation time to be noticeable.

Popular languages in this domain include C++, Fortran, Matlab, Python, Julia, R, etc. They all have their individual strengths and weeknesses. Very, very roughly, they can be divided into languages suitable for prototyping and experimentation. They tend to be dynamically typed and used quite interactively with a REPL, but their numeric performance is not always that impressive. The other group of languages tend to be more suitable for production, the compiler can more easily optimize their code, they tend to be statically typed. But they are usually less interactive because of the compilation which can take a significant amount of time (notably in C++ code with lots of templates). Often, it is not easy to move code from one computer to another, either because of system specific libraries, machine architectures or because of a dependence on an environment being installed such as Matlab.

Clojure stands apart from these languages in that it is both highly interactive, suitable for production, and that its code is very portable. It was not necessarily designed for the kind of numerical computations that we are referring to, but by leveraging the unique characteristics of Clojure such as macros and ```eval```, Lime seeks to provide a practical library with both good performance and expressivity when we need it.

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


## Roadmap

  * ~Factor out the specs~
  * ~Refactor out the seed class~
  * ~Rewrite seed create to a function that takes a function (rename initialize-seed).~
    - ~It accepts a :dirty? flag~
    - ~It will add the expression to the *scope set*~
  * ~Factor out the expr-map class~
  * ~Introduce a **scope** concept~
  * Generate JAVA code

### Binding and sideeffects

A value can be 
  * Not bound if:
    - No sideeffect, or
    - Only used once
  * Bound if:
    - It has a value and a sideeffect, or
    - Complex and used multiple times
  * Rendered in a do-form if
    - It only has a sideeffect.

### Scopes

*PURPOSE*: To introduce dependency hierarchies between nodes, so that they are compiled in the right order. 
Notably, all nodes inside a scope are compiled together and not interleaved with nodes from outside the scope.

A scope is a macro that introduces a 

  * root node
  * termination node

There is a global set, so called *scope set* of expressions

  * That contains all values we should depend upon
  * Whenever we enter a new scope:
    - ~The scope set is pushed onto a backup stack~
    - ~A root node is created for the scope~
    - ~The root node depends on the previous scope set~
    - ~The root node becomes the new scope set~
  * Whenever we exit a scope:
    - ~The termination node depends on the scope set~
    - ~The previous scope set is popped from the backup stack~
    - ~The termination node is added to that set.~
  * The compiled expression of the termination node will hold the result of the scope compilation.

  * ~Tweak ```compile-until``` according to the pseudocode, so that scopes are handled well.~


What about dirties, and scopes?

  * ~The termination node of a scope is dirty (by default, unless overridden) ~
    ~if any dirty operations are carried out inside the scope.~

  * ~When exiting the scope, a flag controls whether the scope itself~
    ~becomes the next dirty, or whether the dirty before entering the scope~
    ~will be the dirty. For if-branches, we want the predecessor to be the dirty, but~
    ~for the entire if-statement, we also want the if-statement to be dirty.~

  * ~The dirties of the scope are terminated (using terminate-snapshot)~
    ~before we leave the scope.~

Where is terminate-snapshot to be used?

To think about:

  * ~When if is used to test loop condition, it should not be bound.~

  * ~The root node of a scope should not be compiled.~

  * ~All scope-added dependencies should be ignored when determining if a variable~
    ~should be bound.~

  * ~Don't forget to terminate the snapshot, when approprate ~
    ~(see terminate-loop-snapshot).~

#### If-statement

Yttersta scope {:dirtify? true}
    True-branch {:dirtify? false}
    False-branch {:dirtify? false}

#### Loops

Form ```{:init ()  :eval-state (fn..) :next (fn ...) :result (fn...)}```

Yttersta scope {:dirtify? true}
    Eval-state-scope {:dirtify? true}
    If-scope: {:dirtify? true, from the if}
        Result
        Next

## Documentation

Use the ```^:no-doc``` on symbols that should be excluded
https://github.com/weavejester/codox#metadata-options

## License

Copyright © 2017 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
