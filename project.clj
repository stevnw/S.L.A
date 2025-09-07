(defproject SLA "1.2"
  :description "Flashcard and Quiz tool for language learning."
  :dependencies [[org.clojure/clojure "1.11.1"]]
  :main SLA.core
  :aot [SLA.core]
  :profiles {:uberjar {:aot :all}}
  :jvm-opts ["-Xms256m" "-Xmx512m"])
