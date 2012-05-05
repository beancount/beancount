(ns beancount.web
  (:use compojure.core
        ring.adapter.jetty)
  (:require [compojure.route :as route]))

(defroutes example
  (GET "/" [] "<h1>Hello World Wide Web!!!</h1>")
  (GET "/accounts.html" [] (format "<html><body>%s</body></html>" (beancount.core/str-accounts beancount.core/accounts)))
  (route/not-found "Page not found"))

(run-jetty (var example) {:port 8080})

