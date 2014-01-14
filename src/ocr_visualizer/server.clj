(ns ocr-visualizer.server
  (:use [ring.middleware.resource :only [wrap-resource]]
        [ring.middleware.file-info :only [wrap-file-info]]
        [compojure.core]
        [clojure.core.matrix]
        [ocr-visualizer.error-count])
  (:require [compojure.route :as route]
            [me.raynes.laser :as l]
            [clojure.java.io :refer [file]]))


(defn index-site []
  (let [a  (slurp "resources/public/ground-truth/186498 meld.txt") 
        b  (slurp "resources/public/ocr-results/186498 tess.txt")
        edits (slurp "resources/public/edits/186498 edits.txt")]
    (l/document (l/parse (file "resources/public/indey.html"))
                (l/id= "left") (l/content (l/unescaped a))
                (l/id= "right") (l/content (l/unescaped b))
                (l/id= "errors") (l/content edits))))



(defroutes handler
  (GET "/index.html" [] (index-site))
  (route/not-found (index-site)))

  

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
             ;(wrap-file-info)
             (wrap-index)))
  
