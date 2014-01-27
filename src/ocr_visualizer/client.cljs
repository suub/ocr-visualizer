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
(def highlights-left (atom []))

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
(def text-left (atom []))
(def text-right (atom []))

(defn highlight
  ([text] (highlight text "blue"))
  ([text color]
     (str "<span style=\"background-color:" color "\">" (if (= text "")
                                                          "|" text) "</span>")))



(defn save-texts [r]
  (reset! text-left (map #(d/html (d/by-id (str "left-" %))) r))
  (reset! text-right (map #(d/html (d/by-id (str "right-" %))) r)))


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
   
(defn fill-highlights [r]
  (let [errors (map #(reader/read-string (d/html (d/by-id (str "errors-" %)))) r)
        hl (map (fn [errors]
                  (for [e errors]
                    (build-highlight e))) errors)]
    (reset! highlights-left (map (fn [hl] (distinct (map first hl))) hl))
    (reset! highlights-right (map (fn [hl] (distinct (map second hl))) hl))))

(defn show-highlights [r]
  (dotimes [i (count r)]
    (d/set-html! (d/by-id (str "left-" i)) (highlight-text (nth @text-left i)
                                                           (nth @highlights-left i)))
    (d/set-html! (d/by-id (str "right-" i)) (highlight-text (nth @text-right i)
                                                            (nth @highlights-right i)))))


(defn fill-table [r]
  (dotimes [i (count r)]
    (let [errors (reader/read-string (d/html (d/by-id (str "errors-" i))))
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
          table (d/html (d/by-id (str "table-" i)))]
      (d/set-html! (d/by-id (str "table-" i))
                   (str (.substring table 0 (- (count table) 8))
                        (apply str rows))))))

(defn visualize-errors []
  (let [i (range (count (d/by-class "wrap")))]
    (save-texts i)
    (fill-highlights i)
    (show-highlights i)
    (fill-table i)))


(set! (.-onload js/window)
      #(do
         (repl-connect)
         (visualize-errors)
         ))

