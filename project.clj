(defproject geex "0.6.0-SNAPSHOT"
  :description "Generative Expressions (Geex) is a code generation tool for writing high-level Clojure code that generates fast low-level code."
  
  :url "https://github.com/jonasseglare/geex"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  ;; run `lein codox` to produce documentation.
  :codox {:metadata {:doc/format :markdown}}

  ;:java-source-paths ["src/java"]
  :source-paths ["src/clj"]
  :java-source-paths ["src/java"]

  :test-paths [
               "test"
               ]

  :aot :all
  
  :dependencies [;; Clojure version to use
                 [org.clojure/clojure "1.10.0"]
                 
                 ;; Utility library
                 [bluebell/utils "0.1.9-SNAPSHOT"]

                 ;; Embeddable Java compiler
                 [org.codehaus.janino/janino "3.0.8"]

                 ;; Java code source formatter
                 [com.google.googlejavaformat/google-java-format
                  "1.6"]])
