(ns ocr-visualizer.server
  (:use [ring.middleware.resource :only [wrap-resource]]
        [ring.middleware.file-info :only [wrap-file-info]]
        [ring.adapter.jetty :as jetty]
        [compojure.core]
        [clojure.core.matrix]
        [ocr-visualizer.error-count])
  (:require [compojure.route :as route]
            [me.raynes.laser :as l]
            [clojure.java.io :refer :all])
  (:gen-class))


(defn fill-page [a b edits num]
  (l/at (l/parse (file "resources/public/page.html"))
        (l/id= "table") (l/id (str "table-" num))
        (l/id= "left") (comp (l/content (l/unescaped (slurp  a)))
                             (l/id (str "left-" num)))
        (l/id= "right") (comp (l/content (l/unescaped (slurp  b)))
                              (l/id (str "right-" num)))
        (l/id= "errors") (comp (l/content (l/unescaped (slurp  edits)))
                               (l/id (str "errors-" num)))))

(defn get-files-sorted [dir]
  (->> (file-seq (file dir))
       rest
       (sort-by #(.getName %))))



(defn index-site []
  (l/document (l/parse (file "resources/public/indey.html"))
              (l/id= "pages")
              (fn [_]
                (map fill-page
                     (get-files-sorted "resources/public/ground-truth")
                     (get-files-sorted "resources/public/ocr-results")
                     (get-files-sorted "resources/public/edits")
                     (range)))))



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
  


(defn -main [port]
  (jetty/run-jetty app {:port (Integer. port) :join? false}))