(defproject geex "0.9.1-SNAPSHOT"
  :description "Generative Expressions (Geex) is a code generation tool for writing high-level Clojure code that generates fast low-level code."
  
  :url "https://github.com/jonasseglare/geex"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  ;; run `lein codox` to produce documentation.
  :codox {:metadata {:doc/format :markdown}}

  :source-paths ["src/clj"]
  :java-source-paths ["src/java"]

  :test-paths ["test"]

  :aot :all

  :javac-options ["-Xlint:unchecked" "-Xlint:deprecation"
                  "-target" "1.8" "-source" "1.8"]

  ;; Used to decide whether to compile on-the-fly or import.
  :profiles {:dev        {:jvm-opts ["-Dgeex_mode=development"]} ; OK
             :test       {:jvm-opts ["-Dgeex_mode=test"]}
             :production {:jvm-opts ["-Dgeex_mode=production"]} 
             :repl       {:jvm-opts ["-Dgeex_mode=repl"]}        ; OK --- Use this to check
             :uberjar    {:jvm-opts ["-Dgeex_mode=uberjar"]}     ; OK
             }

  
  :jvm-opts [;; To avoid stack overflow when compiling bloated code...
             ;; (needed for the N-body example)
             "-Xss16M"

             ;; Where generated Geex code should be put
             "-Dgeex_java_output_path=/tmp/geexjava"]
  
  :dependencies
  [ ;; Clojure version to use
   [org.clojure/clojure "1.10.0"]
                 
   ;; Utility library
   [bluebell/utils "0.1.11"]

   ;; Embeddable Java compiler
   [org.codehaus.janino/janino "3.0.8"]

   ;; Java code source formatter
   [com.google.googlejavaformat/google-java-format "1.6"]])
