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
[geex "0.5.0"]
```

You can also try it out by cloning this repository and looking at [the examples](test/examples), such as 
  * How to [compute the square root](test/examples/sqrt_test.clj) using Newton-Raphson
  * Forward-mode [automatic differentiation](test/examples/ad_test.clj)
  * [Circle fitting](test/examples/circle_fit_test.clj)
  * [Expression templates](test/examples/expr_templates_test.clj) on vectors à la C++
  * More [matrix operations](test/examples/matrix_test.clj) (without expression templates).

## Benchmarks

Have been done, will be published.

## Usage

TODO: Would be nice to have.

See the examples under the *Getting started* section.

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

Important or easy to implement:
 * Neat way of saving Java to disk *or* loading it directly with Janino (environment variable switch or something).
 * Make sure that geex.common/check works (I think it does).
 * String concatenation
 * nth to access chars only works with integer seeds.
 * Check arity when calling Java operators
 * Copy extra examples from other code (router, cljd)
 * Make generated code formatting optional.

Less important:
 * Strip away operations without sideeffects if they are not needed.
 * Unite identical pure operations.

# Contributions

You can contribute by filing bug issues. Fixing minor issues such as failing builds for different platforms is also important.

## License

Copyright © 2019 Jonas Östlund

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
