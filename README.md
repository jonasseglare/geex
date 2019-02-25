# Generative Expressions

Generative Expressions (Geex) is a code generation tool for writing high-level Clojure code that generates fast low-level code.

## About the state of this library

This library is currently quite unstable and is lacking in some areas, notably:

  * Documentation
  * Testing
  * Cleanliness of the code
  * Lack of some basic features

All these points are currently top priorities for this library.

## Getting started

Geex can be obtained as a Maven dependency, so in your Leiningen project, you just have to add the line
```clj
[geex "0.1.0"]
```
Once this is done, you can try it out by creating a new source file:

## Benchmarks

Have been done, will be published.

## Usage

TODO: Would be nice to have.

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

Collected here:

 * Protect EBMD-functions against nil. They create horrible stack traces now.
 * Make sure that geex.common/check works (I think it does).
 * String concatenation
 * Neat way of saving Java to disk *or* loading it directly with Janino (environment variable switch or something).
 * nth to access chars only works with integer seeds.


## License

Copyright © 2019 Jonas Östlund

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
