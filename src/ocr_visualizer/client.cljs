(ns ocr-visualizer.client2
  (:require [domina :as d]
            [cljs.reader :as reader]
            [clojure.string :as string]
            [clojure.browser.repl :as repl]))

;;************************************************
;; Dev stuff
;;************************************************
(def dev-mode true)

(defn repl-connect [] 
 (when dev-mode
   (repl/connect "http://localhost:9000/repl")))




(defn get-errors []
  (-> (d/by-id "errors") d/text reader/read-string))

(defn get-left []
  (-> (d/by-id "left") d/html))

(defn get-right []
  (-> (d/by-id "right") d/html))




;;highlights are of the form start end color
(def highlights-left (atom []))(ns ocr-visualizer.client
  (:require [domina :as d]
            [cljs.reader :as reader]
            [clojure.string :as string]
            [clojure.browser.repl :as repl]))

;;************************************************
;; Dev stuff
;;************************************************
(def dev-mode true)

(defn repl-connect [] 
 (when dev-mode
   (repl/connect "http://localhost:9000/repl")))




(defn get-errors []
  (-> (d/by-id "errors") d/text reader/read-string))

(defn get-left []
  (-> (d/by-id "left") d/html))

(defn get-right []
  (-> (d/by-id "right") d/html))


;;highlights are of the form start end color
(def highlights-left (atom []))
(def highlights-right(atom []))
(def text-left (atom ""))
(def text-right (atom ""))

(defn highlight
  ([text] (highlight text "blue"))
  ([text color]
     (str "<span style=\"background-color:" color ">" text "</span>")))

(def offset 43)


(defn highlight-text [text positions]
  (apply str
         (-> (reduce (fn [[pos substrings] [nstart nend color]]
                       [nend (conj substrings (.substring text pos nstart)
                                   (highlight (.substring text nstart nend)
                                              color))])
                     [0 []] positions)
             second
             (conj (.substring text (second (last positions)) (count text))))))

(defn save-texts []
  (reset! text-left (d/html (d/by-id "left")))
  (reset! text-right (d/html (d/by-id "right"))))

(defn fill-highlights []
  (let [errors (reader/read-string (d/html (d/by-id "errors")))
        insertions-left (distinct (map first (:insertions errors)))
        insertions-right (distinct (map second (:insertions errors)))]
    (reset! highlights-left (for [i insertions-left] [i (inc i) "blue"]))
    (reset! highlights-right (for [i insertions-right] [(dec i) i "blue"]))))

(defn show-highlights []
  (d/set-html! (d/by-id "left") (highlight-text @text-left @highlights-left))
  (d/set-html! (d/by-id "right") (highlight-text @text-right @highlights-right)))

(set! (.-onload js/window)
      #(do
         (repl-connect)
         (save-texts)
         (fill-highlights)
         (show-highlights)))
(def highlights-right(atom []))
(def text-left (atom ""))
(def text-right (atom ""))

(defn highlight
  ([text] (highlight text "blue"))
  ([text color]
     (str "<span style=\"background-color:" color "\">" text "</span>")))

(def offset 43)


(defn highlight-text [text positions]
  (apply str
         (-> (reduce (fn [[pos substrings] [nstart nend color]]
                       [nend (conj substrings (.substring text pos nstart)
                                   (highlight (.substring text nstart nend)
                                              color))])
                     [0 []] positions)
             second
             (conj (.substring text (second (last positions)) (count text))))))

(defn save-texts []
  (reset! text-left (d/html (d/by-id "left")))
  (reset! text-right (d/html (d/by-id "right"))))

(defn fill-highlights []
  (let [errors (reader/read-string (d/html (d/by-id "errors")))
        insertions-left (map first (:insertions errors))
        insertions-right (map second (:insertions errors))
        deletions-left (map first (:deletions errors))
        deletions-right (map second (:deletions errors))
        substitutions-left (map first (:substitutions errors))
        substitutions-right (map second (:substitutions errors))
        ]
    (reset! highlights-left
            (sort-by first
                     (concat
                      (for [i insertions-left] [(dec i) i "blue"])
                      (for [i deletions-left] [(dec i) i "red"])
                      (for [i substitutions-left] [(dec i) i "green"]))))
    (reset! highlights-right
            (sort-by first
                     (concat
                      (for [i insertions-right] [(dec i) i "blue"])
                      (for [i deletions-right] [(dec i) i "red"])
                      (for [i substitutions-right] [(dec i) i "green"]))))))

(defn show-highlights []
  (d/set-html! (d/by-id "left") (highlight-text @text-left @highlights-left))
  (d/set-html! (d/by-id "right") (highlight-text @text-right @highlights-right)))

(set! (.-onload js/window)
      #(do
         (repl-connect)
         (save-texts)
         (fill-highlights)
         (show-highlights)))
