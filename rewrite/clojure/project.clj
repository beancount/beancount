(defproject rocky "1.0.0-SNAPSHOT"
  :description "Beanjure"

  :dependencies [
                 [org.eclipse.jetty/jetty-websocket "7.6.1.v20120215"] ;; Version compatible with ring.
                 [org.eclipse.jetty/jetty-servlet "7.6.1.v20120215"]
                 ;;[ord.joda.time "MASTER"]
                 ;;;;[org.cometd.java/cometd-websocket-jetty "2.4.0"]
                 ;;;;[org.cometd.java/cometd-java-server "2.4.0"]

                 ;; Web application framework for frontend.
                 ;; Note: we need 1.1.0 for jetty7 support (which has websockets).
                 ;; [ring/ring "1.1.0-beta2"]
                 ;; [ring/ring-core "1.1.0-beta2"]
                 ;; [ring/ring-jetty-adapter "1.1.0-beta2"]
                 ;;[ring/ring-servlet "1.1.0-beta2"]
                 ;; [compojure "1.0.1"]
                 ;; [hiccup "1.0.0-beta1"]
                 ;; [enlive "1.0.0-SNAPSHOT"]

                 ]
  :plugins [[lein-swank "1.4.1"]]
  )
