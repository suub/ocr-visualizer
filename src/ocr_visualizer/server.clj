(ns ocr-visualizer.server
  (:use [ring.middleware.resource :only [wrap-resource]]
        [ring.middleware.file-info :only [wrap-file-info]]))


(defn handler [request] {:status 200})

;handling routing "/" -> "/index.html"
(defn wrap-index [handler]
  (fn [req]
    (println (pr-str req))
    (if (= (:uri req) "/")  
      (handler (assoc req :uri "/index.html"))
      (handler req))))

;setting up a simple resource handler for ring
(def app (-> handler
             (wrap-resource "public")
             (wrap-file-info)
             (wrap-index)))
  
