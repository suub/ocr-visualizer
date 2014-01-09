(ns ocr-visualizer.server
  (:use [ring.middleware.resource :only [wrap-resource]]
        [ring.middleware.file-info :only [wrap-file-info]]
        [compojure.core]
        [clojure.core.matrix]
        [ocr-visualizer.error-count])
  (:require [compojure.route :as route]
            [me.raynes.laser :as l]
            [clojure.java.io :refer [file]]))


(defn template []
  (let [a (slurp "resources/public/ground-truth/186498 meld.txt")
        b (slurp "resources/public/ocr-results/186498 tess.txt")
        _ (new-matrix :ndarray 2 2)
        _ (prn "initialized nd-array")]
    (l/document (l/parse (file "resources/public/template.html"))
                (l/id= "left") (l/content a)
                (l/id= "right") (l/content b)
                (l/id= "errors") (l/content (pr-str "hi" (edits a b))))))

(defroutes handler
  (GET "/index.html" [] "<h1>Hello World</h1>")
  (route/not-found (template) #_"<h1>Page not fojkjund</h1>"))

  

;handling routing "/" -> "/index.html"
(defn wrap-index [handler]
  (fn [req]
    (println (pr-str req))
    (if (= (:uri req) "/")  
      (handler (assoc req :uri "/index.html"))
      (handler req))))

;setting up a simple resource handler for ring
(def app (-> #'handler
             (wrap-resource "public")
             (wrap-file-info)
             (wrap-index)))
  
