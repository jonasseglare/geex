# Generative Expressions

Geex, short for *generative expressions*, is a library to write code that writes code and, in particular, fast code. A generative expression is a Clojure expression that evaluates to a *streamlined representation* of the final computation to be performed without the extra overhead of function calls and short-lived intermediate datastructures. From the generated representation, low level code can be rendered and compiled into a very fast implementation.

Geex is ultimately intended to be a practical tool to write elegant high-level and and high-performance code, accessible to anyone who already knows Clojure. I hope you will enjoy it!

## Rationale

As computers are getting faster we can solve computationally bigger problems in domains such as numerical optimization, computer vision and machine learning. These are examples of domains where the main bottleneck is often the computational power of the computer more than, for example, network bandwidth. To take advantage of this computational power, the software that runs on it plays a role too. 

Writing software tends to be a trade-off between programmer productivity and the speed at which the computer can execute it. Typically, higher level code tends to run slower. Because of this, there is often a strong temptation for so called *premature optimization* where the programmer writes code that is more complex, less clear or less maintainable out of fear that the implementation will otherwise be too slow. For instance, due to distrust that the compiler will not perform adequate inlining, a programmer may produce a big monolithic code block and unroll loops manually instead of factoring it into smaller functions.

There are languages where high-level constructs offer good performance when used idiomatically, according to the *zero overhead principle*. This is because the compiler can take advantage of an abundance of statically known information and perform a lot of precomputations during compilation. However, these languages are often perceived complex and require more expertise to use effectively. Long compilation times and slow feedback make them less productive tools.

Geex seeks to combine the programmer productivity, simplicity and robustness of Clojure with high performance. First, it achieves productivity and simplicity by remaining symbiotic with and reusing as much as possible of the rest of the Clojure ecosystem and its host platform. Technically, Geex code is just Clojure code and it can be freely mixed with non-Geex Clojure code without extra glue code in order to get the best of both worlds. Second, it achieves high performance à la carte through lightweight and embedded generative programming provided as a library. In short, we can write efficient programs without giving up high-level constructs such as functions and immutable datastructures.

## Getting started

Geex can be obtained as a Maven dependency, so in your Leiningen project, you just have to add the line
```clj
[geex "0.1.0-SNAPSHOT"]
```
Once this is done, you can try it out by creating a new source file:

## Benchmarks

## Usage

## API Reference

The documentation can be generated using ```lein codox```.

Use the ```^:no-doc``` on symbols that should be excluded
https://github.com/weavejester/codox#metadata-options

### Module structure
If you add ```[lein-ns-dep-graph "0.2.0-SNAPSHOT"]``` to your Leiningen plugins, this graph can be generated using ```lein ns-dep-graph```:
![Module graph](ns-dep-graph.png)

```geex.core``` contains the core components of code generation and analysis.

```geex.base``` is a base library of common operations.

```geex.java``` contains specific support for the Java platform.

## License

Copyright © 2018 Jonas Östlund

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
