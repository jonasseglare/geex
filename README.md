# Geex—Generative Expressions

Geex is a library for writing simple and composable code that generates high-performance low level code, suitable for applications in optimization, image processing, machine learning, computer vision, graphics, games, statistics and many other domains.

It achieves this using a statically typed value oriented approach where every part of the generated program is expressed using immutable data structures. These values can be manipulated however we like using all of Clojure, giving us both metaprogramming powers and simplicity.

The objective of Geex is to combine the productivity, simplicity and reach of Clojure with improved performance for computationally intense applications.

## What problem does Geex solve?

Geex is specifically designed for solving numerical problems common in optimization, engineering, computer vision, machine learning, etc. Problems where we work with matrices, minimize objective functions, simulate things, integrate things, and so on, where there is enough data to crunch for the computation time to be noticeable.

Although recently, new languages have popped up that claim to address the so-called "two-language problem" (one language that is fast to write code in, and one language whose code tends to execute fast), real-world problems are really *N-language* problems: It is not just a question of development vs runtime speed. There are so many other aspects too, such as deployability on different platforms and what libraries are available. Real-world problems are typically not only matter of how fast we can solve an optimization problem, but also how practical it is to build a GUI to use the algorithm, interacting with databases, calling other computers over HTTP, and so on. Clojure is already a very capable language for addressing a wide range of problems with its good interoperability combined with a simple value oriented approach. With Geex it gains a significant speed boost that makes it practical to use Clojure to also address hard problems in numerical computing.

## Usage

Try out the [Gorilla repl worksheet tutorial]() or [read the PDF]().

## Documentation
If you add ```[lein-ns-dep-graph "0.2.0-SNAPSHOT"]``` to your Leiningen plugins, this graph can be generated using ```lein ns-dep-graph```:
![Module graph](ns-dep-graph.png)

Brief description:
```geex.java``` is the file to include if you want to use Java as the platform to which code is generated. This file also extends the set-dispatch-methods of ```geex.platform.high``` and ```geex.platform.low```.

```geex.platform.high``` are all high-level operations that we may want to access from different platforms. They are high-level in the sense that they depend on the ```lime.core``` module.

```geex.platform.low``` are all low-level operations that vary from platform to platform. They are low-level in the sense that they don't require the ```geex.core``` module.

```geex.core``` is the main implementation of most common stuff of geex, notable code generation.

```geex.core.exprmap``` is mainly the graph and compilation state of the compiler. Unless you are doing something special, most of the time you should not have to deal with this.


Use the ```^:no-doc``` on symbols that should be excluded
https://github.com/weavejester/codox#metadata-options

## License

Copyright © 2018 Jonas Östlund

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
