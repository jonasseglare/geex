(defproject sample-project "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [geex "0.10.2"]]

  :source-paths ["src/clj"]
  :java-source-paths ["src/java"]

  
  :aot :all

  :profiles {:dev        {:jvm-opts ["-Dgeex_mode=development"]} ; OK
             :test       {:jvm-opts ["-Dgeex_mode=test"]}
             :production {:jvm-opts ["-Dgeex_mode=production"]} 
             :repl       {:jvm-opts ["-Dgeex_mode=repl"]}        ; OK --- Use this to check
             :uberjar    {:jvm-opts ["-Dgeex_mode=uberjar"]}     ; OK
             }

  :jvm-opts [
             ;; Where generated Geex code should be put
             "-Dgeex_java_output_path=src/java"]

  :main sample-project.core

  :omit-source true
  
  :repl-options {:init-ns sample-project.core})
