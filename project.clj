(defproject lime "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :java-source-paths ["java/"]
  :plugins [[lein-nodisassemble "0.1.3"]]
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [com.clojure-goes-fast/clj-java-decompiler "0.1.0"]
                 [proteus "0.1.6"]
                 [primitive-math "0.1.6"]
                 [bluebell/utils "0.1.3-SNAPSHOT"]])
