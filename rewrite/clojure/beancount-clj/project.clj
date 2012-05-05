;; Version for Clojure and contrib.
(def version "1.2.0") ;; "1.2.0-master-SNAPSHOT"

(defproject beancount "1.0.0-SNAPSHOT"
  :description "Beancount bookkeeping software rewritten in Clojure."
  :dependencies [[org.clojure/clojure ~version]
                 [org.clojure/clojure-contrib ~version]
                 [compojure "0.4.0"]
                 [ring/ring-jetty-adapter "0.2.3"]
		 ]
  :dev-dependencies [[swank-clojure "1.3.0-SNAPSHOT"]]
  )
