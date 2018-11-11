# Geex—Generative Expressions

Geex, short for *generative expressions*, is a library to write code that writes code and, in particular, fast code. A generative expression is a Clojure expression that generates a streamlined *representation* of the final computation to be performed, where most of the overhead of accessing immutable collections or calling functions has been removed. From the generated representation, low level code can be rendered and compiled into a very fast implementation.

Geex is ultimately intended to be a practical tool to write elegant high-level and and high-performance code. I hope you will enjoy it!

## Rationale

As computers are getting faster we can solve computationally bigger problems in domains such as numerical optimization, computer vision and machine learning. These are examples of domains where the main bottleneck is often the computational power of the computer more than, for example, network bandwidth. To take advantage of this computational power, the software that runs on it plays a role too. It is often the case that writing this software is a trade-off between programmer productivity and the speed at which the computer can execute it. Typically, higher level code tends to run slower. 

Geex seeks to combine the programmer productivity, simplicity and robustness of Clojure with high performance through code generation. This will make it easier to quickly produce efficient solutions to computationally intensive problems while taking advantage of the portability, ecosystem and interop of platforms such as the JVM.

## Getting started

Geex can be obtained as a Maven dependency, so in your Leiningen project, you just have to add the line
```
[geex "0.1.0-SNAPSHOT"]
```
Once this is done, you can try it out by creating a new source file:

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
