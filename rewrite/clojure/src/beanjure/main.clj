(ns beanjure.main
  (:use ring.adapter.jetty
        ring.middleware.stacktrace
	compojure.core
        )

  (:require [compojure.route :as route]
            [compojure.handler :as handler]
	    [ring.middleware.file :as file]
	    [ring.middleware.reload :as reload]
	    [ring.middleware.stacktrace :as stacktrace]
            )
  )

(defroutes main-routes
  (GET "/" [] "<h1>Beancount</h1>")
  (route/files "/home/blais/p/beanjure/web" :root "/r")
  (route/resources "/resources")
  (route/not-found "Page not found")
  )


(defn- log [msg & vals]
  (let [line (apply format msg vals)]
    (locking System/out (println line))))

(defn wrap-request-logging [handler]
  (fn [{:keys [request-method uri] :as req}]
    (let [resp (handler req)]
      (log "Processing %s %s" request-method uri)
      resp)))



(def app
  (-> (handler/site #'main-routes)
      (reload/wrap-reload '(beanjure.main))
      (file/wrap-file "/home/blais/p/beanjure/web")
      (stacktrace/wrap-stacktrace)
      (wrap-request-logging)

      ;; (wrap-file-info)
      ;; (wrap-request-logging)
      ;; (wrap-reload '[adder.middleware adder.core])
      ;; (wrap-bounce-favicon)
      ;; (wrap-stacktrace)))
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

(def server (run-server #'app 8000))
(.stop server)

(require '[clojure.pprint :as pprint]
         '[clojure.reflect :only (reflect)])

(pprint/pprint (reflect server))

(def f (route/files "/home/blais/p/beanjure/web" :root "/r"))
(f nil)

(use 'clojure.set)
(clojure.set/difference #{1 2} #{2 3})

(use 'swank.cdt)
(set-bp clojure.set/difference)

(set-bp stop-server)


(def example-route (GET "/" [] "<html>...</html>"))
(example-route {:server-port 80
		:server-name "127.0.0.1"
		:remote-addr "127.0.0.1"
		:uri "/"
		:scheme :http
		:headers {}
		:request-method :get})



)
