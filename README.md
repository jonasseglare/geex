# Generative Expressions

Generative Expressions (Geex) is a code generation tool for writing high-level Clojure code that generates fast low-level code.

## Getting started

Geex can be obtained as a Maven dependency, so in your Leiningen project, you just have to add the line
```clj
[geex "0.1.0"]
```
Once this is done, you can try it out by creating a new source file:

## Benchmarks

TODO. 

But see the https://github.com/jonasseglare/pres-clojured2019 and in particular [latex/top.pdf](https://github.com/jonasseglare/pres-clojured2019/blob/master/latex/top.pdf) for some plots.

## Usage

TODO: Would be nice to have 

## API Reference

The documentation can be generated using ```lein codox```.

Use the ```^:no-doc``` on symbols that should be excluded
https://github.com/weavejester/codox#metadata-options

### Module structure
If you add ```[lein-ns-dep-graph "0.2.0-SNAPSHOT"]``` to your Leiningen plugins, this graph can be generated using ```lein ns-dep-graph```:
![Module graph](ns-dep-graph.png)

```geex.core``` contains the core components of code generation and analysis.

```geex.common``` is a library of common operations.

```geex.java``` contains specific support for the Java platform.

# Issues and features to address

 * Protect EBMD-functions against nil. They create horrible stack traces now.
 * Make sure that geex.common/check works (I think it does).
 * String concatenation
 * Save Java to disk instead of directly loading it in Janino...


## License

Copyright © 2018 Jonas Östlund

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
