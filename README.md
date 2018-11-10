# Geex—Generative Expressions

Geex, short for *generative expressions*, is a library to write code that writes code and, in particular, fast code. A generative expression is a Clojure expression that generates a streamlined *representation* of the final computation to be performed, where most of the overhead of accessing immutable collections or calling functions has been removed. From the generated representation, low level code can be rendered and compiled into a very fast implementation.

## Rationale

As computers are getting faster we can solve computationally bigger problems in domains such as numerical optimization, computer vision and machine learning. These are examples of domains where the main bottleneck is often the computational power of the computer more than, for example, network bandwidth. To take advantage of this computational power, the software that runs on it plays a role too. It is often the case that writing this software is a trade-off between programmer productivity and the speed at which the computer can execute it. Typically, higher level code tends to run slower. 

Geex seeks to combine the programmer productivity, simplicity and robustness of Clojure with high performance through code generation. This will make it easier to quickly produce efficient solutions to computationally intensive problems and run the code on standard platforms such as the JVM.

## How does it work?

## Usage

Try out the [Gorilla repl worksheet tutorial]() or [read the PDF]().

## Documentation

The documentation can be generated using ```lein codox```.

Use the ```^:no-doc``` on symbols that should be excluded
https://github.com/weavejester/codox#metadata-options

### Module structure
If you add ```[lein-ns-dep-graph "0.2.0-SNAPSHOT"]``` to your Leiningen plugins, this graph can be generated using ```lein ns-dep-graph```:
![Module graph](ns-dep-graph.png)

```geex.java``` is the file to include if you want to use Java as the platform to which code is generated. This file also extends the set-dispatch-methods of ```geex.platform.high``` and ```geex.platform.low```.

```geex.platform.high``` are all high-level operations that we may want to access from different platforms. They are high-level in the sense that they depend on the ```lime.core``` module.

```geex.platform.low``` are all low-level operations that vary from platform to platform. They are low-level in the sense that they don't require the ```geex.core``` module.

```geex.core``` is the main implementation of most common stuff of geex, notable code generation.

```geex.core.exprmap``` is mainly the graph and compilation state of the compiler. Unless you are doing something special, most of the time you should not have to deal with this.

## License

Copyright © 2018 Jonas Östlund

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
