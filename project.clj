(defproject cs499 "0.1.0-SNAPSHOT"
  :description "CS499 Web site"
  :url "http://cs499.bbirec.com"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/math.numeric-tower "0.0.1"]
                 [noir "1.2.1"]
                 [factual/factual-clojure-driver "1.4.3"]
                 [factql "1.0.3"]
                 [com.novemberain/monger "1.1.2"]]
  :main cs499.server)