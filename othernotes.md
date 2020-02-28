## Issues and features to address

Important or easy to implement:
 * Simplifications of the core:
   - Sort out the recur-spaghetti... or simplify it.
   - Fewer mutations, if possible. Concentrate them to SeedState.
 * Sequence stuff: drop, take, take-while, filter, etc.
 * array and vec functions: Export a lazy seq to a vector.
 * Refactor, so that a seed has typeInfo, instead of type. And typeInfo wraps the type with meda data.

Less important:
 * Provide JavaSourceClassLoader as parent class loader to SimpleCompiler. This will allow it to use previously compiled classes. Or maybe better, whenever we cook something, we set the parent class loader to be the new class loader.
 * A class registry, mapping full class name to a function producing a class. It will first look in the class loader (which is a JavaSourceClassLoader). This registry is used for structs and struct arrays, so that structurally equivalent things map the same.
 * An export function that is called on all values returned. By default, it returns the value unchanged. But it can be extended, so that for ::iterables it turns them into lazy seqs, and for matrix expressions it turns them into MDArrays.
 * Implement ILookUp etc for seeds, so that we can apply keywords and nth and count to seeds.
 * Figure out namespace
 * Add dependencies between stateful, respect scope.
 * Prohibit some chars in typed-defn names, such as >, <
 * Strip away operations without sideeffects if they are not needed.
 * Unite identical pure operations.
 * Contiguous structured arrays.
 * Calling super of class that we extend.
 * Cartesian product
 * Better management of homogenization of slightly different values between branches in conditional forms: Maybe, if there is no change in the value, we reset the type???

## Design

### Getting the current namespace

```
(defmacro this-ns []
  (let [s (gensym)]
    `(do (def ~s)
         (-> (var ~s)
             (.ns)
             ;(.getName)
             ;name
             ))))
```

### Structs and Struct array
Add a module ```geex/java/struct```, interface ```IStruct``` and interface ```IStructArray```.

```IStruct``` has methods ```getData```, ```setData```, ```make```, ```getFloat0```, ```getTypeSignature```.
```IStructArray``` has methods ```getData```, ```setData```, ```getStruct```, ```setStruct```, ```getFloats```, ```getDoubles```, ```getTypeSignature```.

A struct is a structural type. Its class can be defined on-the-fly e.g. ```(struct {:a Double})```. In the background, it does the following:
    * Generates a unique name for it based on its shape.
    * In a map, if a java.lang.Class already exists at that key, then return it. Otherwise, save it to disk and load it again with the JavaSourceClassLoader of Janino.

There is also a construct, ```data-class-template```, that lets us defined polymorphic classes, e.g.

```clj
(def-data-class-template my-bbox
  (fn [data-shape]
    {:interfaces [IBBox]
     :methods [{:name "extend"
                :arg-types [Object]
                :fn (fn [this x] ...)}}))
```
and then we can simply call ```(my-bbox {:minv [Double Double] :maxv [Double Double]})``` to generate a specialized class implementing these methods... that has polymorphic behaviour.
