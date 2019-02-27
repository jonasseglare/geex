(defproject geex "0.3.0-SNAPSHOT"
  :description "Geex is a library for writing simple and composable code that generates high-performance low level code, suitable for applications in optimization, image processing, machine learning, computer vision, graphics, games, statistics and many other domains."
  
  :url "http://example.com/FIXME"
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
  
  :dependencies [;; Clojure version to use
                 [org.clojure/clojure "1.10.0"]
                 
                 ;; Utility library
                 [bluebell/utils "0.1.7"]

                 ;; Embeddable Java compiler
                 [org.codehaus.janino/janino "3.0.8"]

                 ;; Java code source formatter
                 [com.google.googlejavaformat/google-java-format "1.6"]])
