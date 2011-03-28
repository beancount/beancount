(ns beanjure.main
  (:use ring.adapter.jetty
        ring.middleware.stacktrace
	compojure.core
	[clojure.contrib.repl-utils :only (show)]
        )

  (:require [compojure.route :as route]
            [compojure.handler :as handler]
	    [ring.middleware.reload :as reload]
	    [ring.middleware.stacktrace :as stacktrace]
	    [ring.middleware.reload-modified :as reload-modified])
  )

(defroutes main-routes
  (GET "/" [] "<h1>Beancount</h1>")
  (route/files "/home/blais/p/beanjure/web" :root "/r")
  (route/resources "/resources")
  (route/not-found "Page not found")
  )


(defn create-app []
  (-> (handler/site #'main-routes)
      (stacktrace/wrap-stacktrace)
      (reload/wrap-reload '(beanjure.main))
      ;;reload/wrap-file "/home/blais/p/beanjure/web"
      ;;reload/wrap-file-info
      ;;(reload-modified/wrap-reload-modified ["/home/blais/p/beanjure/web"])
      ))

;; Runs a server in a background thread.
(defn run-server [handler port]
  (let [server (run-jetty handler {:port port, :join? false})]
    (println "Started server on port" port)
    server))

(defn stop-server [server]
  (.stop server))

;;------------------------------------------------------------------------------

(comment
  (def server (run-server (create-app) 8000))
  (.stop server)

  (show server)

)
