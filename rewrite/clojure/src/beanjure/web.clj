(ns beanjure.web
  (:use ring.adapter.jetty
        ring.middleware.stacktrace
        [compojure.core :only (defroutes GET)])

  (:require [compojure.core :as compo]
            [compojure.route :as route]
            [compojure.handler :as handler]
	    [ring.middleware.file :as file]
	    [ring.middleware.reload :as reload]
	    [ring.middleware.stacktrace :as stacktrace]
            [net.cgrand.enlive-html :as en]
            )

  (:import [java.io File])
  )



(def static-dir
  "Root of static files being served."
  (str (System/getenv "HOME") "/p/beanjure/static"))

(def template-dir
  "Root of template files to load."
  (str (System/getenv "HOME") "/p/beanjure/templates"))



(en/deftemplate layout (File. (str template-dir "/layout.html"))
  []
  )

(defroutes routes
  (GET "/" [] (layout))
  ;;(route/files static-dir)
  ;;(route/resources "/resources")
  (route/not-found "Page not found")
  )



;; FIXME: use java.logging.*, set it up.
(defn- log [msg & vals]
  (let [line (apply format msg vals)]
    (locking System/out (println line))))

(defn wrap-request-logging [handler]
  (fn [{:keys [request-method uri] :as req}]
    (let [resp (handler req)]
      (log "Processing %s %s" request-method uri)
      resp)))




(def app
  (-> (handler/site #'routes)
      (reload/wrap-reload '(beanjure.main))
      (file/wrap-file static-dir)
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

;; (def example-route (GET "/" [] "<html>...</html>"))
;; (example-route {:server-port 80
;; 		:server-name "127.0.0.1"
;; 		:remote-addr "127.0.0.1"
;; 		:uri "/"
;; 		:scheme :http
;; 		:headers {}
;; 		:request-method :get})



)
