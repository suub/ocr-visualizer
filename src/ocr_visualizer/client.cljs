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
     (str "<span style=\"background-color:" color "\">" (if (= text "")
                                                          "|" text) "</span>")))



(defn save-texts []
  (reset! text-left (d/html (d/by-id "left")))
  (reset! text-right (d/html (d/by-id "right"))))


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

(defn build-insertion-highlight [[_ [l r]]]
  [[l l "green"] [r (inc r) "green"]])

(defn build-deletion-highlight [[_ [l r]]]
  [[l (inc l) "red"] [r r "red"]])

(defn build-one-to-many-highlight [[_ [ls rs] [le re]]]
  [[ls le "yellow"] [rs (inc re) "yellow"]])

(defn build-many-to-one-highlight [[_ [ls rs] [le re]]]
  [[ls (inc le) "orange"] [rs re "orange"]])

(defn build-substitution-highlight [[_ [l r]]]
  [[l (inc l) "blue"][r (inc r) "blue"]])

(defn build-highlight [error]
  (let [[a b] (first error)]
    (cond
     (= a 8)  (build-insertion-highlight error)
     (= b 8)  (build-deletion-highlight error)
     (= b 7)  (build-one-to-many-highlight error)
     (= a 7)  (build-many-to-one-highlight error)
     :else    (build-substitution-highlight error))))
   
(defn fill-highlights []
  (let [errors (reader/read-string (d/html (d/by-id "errors")))
        hl (for [e errors]
             (build-highlight e))]
    (reset! highlights-left (distinct (map first hl)))
    (reset! highlights-right (distinct (map second hl)))))
(defn show-highlights []
  (d/set-html! (d/by-id "left") (highlight-text @text-left @highlights-left))
  (d/set-html! (d/by-id "right") (highlight-text @text-right @highlights-right)))


(defn fill-table []
  (let [errors (reader/read-string (d/html (d/by-id "errors")))
        kinds (group-by first errors)
        rows (for [[code positions] (sort-by (comp #(reduce + %) first) kinds)]
               (str "<tr>"
                    "<td>" code "</td>"
                    "<td>" (count positions) "</td>"
                    "<td>" positions "</td>"
                    "<td>" (-> [code (first positions)]
                               build-highlight
                               first
                               (nth 2))
                    "</tr>"))
        table (d/html (d/by-id "table"))]
    (d/set-html! (d/by-id "table")
                 (str (.substring table 0 (- (count table) 8))
                      (apply str rows)))))

(set! (.-onload js/window)
      #(do
         (repl-connect)
         (save-texts)
         (fill-highlights)
         (show-highlights)
         (fill-table)))


