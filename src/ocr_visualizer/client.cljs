(ns ocr-visualizer.client2
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [domina :as d]
            [cljs.reader :as reader]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [ajax.core :refer [GET POST]]
            [cljs.core.async :refer [put! chan <!]]
            [clojure.string :as string]
            [clojure.browser.repl :as repl]))
;;************************************************
;; Dev stuff
;;************************************************

(enable-console-print!)

(+ 1 2 3)

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


(defn build-insertion-highlight [[_ [l r] :as error]]
  [{:type :insertion
    :error error
    :position [l l "green"]}
   {:type :insertion
    :error error
    :position [r (inc r) "green"]}])

(defn build-deletion-highlight [[_ [l r] :as error]]
  [{:type :deletion
    :error error
    :position [l (inc l) "red"]}
   {:type :deletion
    :error error
    :position [r r "red"]}])


(defn build-one-to-many-highlight [[_ [ls rs] [le re] :as error]]
  [{:type :one-to-many
   :error error
   :position [ls le "yellow"]}
   {:type :one-to-many
    :error error
    :position [rs (inc re) "yellow"]}])

(defn build-many-to-one-highlight [[_ [ls rs] [le re] :as error]]
  [{:type :many-to-one
   :error error
   :position [ls (inc le) "orange"]}
   {:type :many-to-one
    :error error
    :position [rs re "orange"]}])

(defn build-substitution-highlight [[_ [l r] :as error]]
  [{:type :substitution
   :error error
   :position [l (inc l) "#00FFFF"]}
   {:type :substitution
    :error error
    :position [r (inc r) "#00FFFF"]}])

(defmulti get-position (fn [a b] a))

(defmethod get-position :substitution [_ [_ [l r]]]
  [[l (inc l)] [r (inc r)]])

(defmethod get-position :many-to-one [_ [_ [ls rs] [le re]]]
  [[ls (inc le)] [rs re]])

(defmethod get-position :one-to-many [_ [_ [ls rs] [le re]]]
  [[ls le] [rs (inc re)]])

(defmethod get-position :insertion [_ [_ [l r]]]
  [[l l] [r (inc r)]])

(defmethod get-position :deletion [_ [_ [l r]]]
  [[l (inc l)] [r r]])


(def empty-sign "|")

(defn get-text [[start end] text]
  #_(prn "get-text " start end text)
  (let [t (.substring text start end)]
    (if-not (= t "")
      t
      empty-sign)))

(defn make-highlight [error type a b page-index]
  #_(prn "make-highlight " a b type page-index)
  (let [positionlr (get-position type error)
        textlr (mapv get-text positionlr [a b])
        color (get {:substitution "#00FFFF" :insertion "green" :deletion "red"
                    :one-to-many "yellow" :many-to-one "orange"} type)]
    (mapv (fn [pos text lr]
            {:type type
             :position pos
             :color color
             :text text
             :error error
             :id (str page-index "-" error "-" lr)}) positionlr textlr ["l" "r"])))

(defn get-type [error]
  (let [[a b] (first error)]
    (cond
      (= a 8)  :insertion
      (= b 8)  :deletion
      (= b 7)  :one-to-many
      (= a 7)  :many-to-one
      :else    :substitution)))

(get-type [[1 1] [3 3]])

(defn build-highlight [error texta textb page-index]
  (let [type (get-type error)]
    (make-highlight error type texta textb page-index)))

(defn highlight-text [text positions]
  (apply str
         (-> (reduce (fn [[pos substrings] [nstart nend color]]
                       [nend (conj substrings (.substring text pos nstart)
                                   (highlight (.substring text nstart nend)
                                              color))])
                     [0 []] positions)
             second
             (conj (.substring text (second (last positions)) (count text))))))


(defn build-highlights [errors a b page-index]
  #_(prn "lights " errors a b)
  (as->
    (reduce (fn [[posl hls-left posr hls-right] error]
              (let [[{[ls le] :position :as hl-l} {[rs re] :position :as hl-r}]
                     (build-highlight error a b page-index)
                   #_#_ _ (prn "bhlths " ls le rs re error a b)]
                [le (conj hls-left (.substring a posl ls) hl-l)
                 re (conj hls-right (.substring b posr rs) hl-r)])) [0 [] 0 []] errors) x
     (let [[le hls re hlr] x]
       [(conj hls (.substring a le (count a))) (conj hlr (.substring b re (count b)))])))



(defn fill-highlights [r]
  (let [errors (map #(reader/read-string (d/html (d/by-id (str "errors-" %)))) r)
        hl (map (fn [errors]
                  (for [e errors]
                    (highlight e))) errors)]
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
                                 highlight
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

(defn compute-highlights [left right error-codes]
  (let [highlights (map light error-codes)
        left-hl (map first highlights)
        right-hl (map second highlights)]))


(def app-state (atom {:pages []
                      :available-pages []
                      #_[{:left "ab" :right "bb" :error-codes '([[1 1] [0 0]])}
                              {:left "439\nalljährlich wurde eine Festrede zu Ehren Kant's gehalten. Rosenkranz liebte es,\njunge Talente um sich zu sammeln; damals wurde es Sitte, sich lyrisch gehn zu\nlassen, zuerst in sentimentalen Trinkliedern, doch spielte bald die Freiheit im All¬\ngemeinen und der aufgeklärte Patriotismus hinein, und es fehlte weder die Heinesche\nZerrissenheit noch die Grün'sche Reflexionspoesie. Die Mehrzahl der Professoren\nhielt auf rationalistische Auffassung der Geschichte, des Rechts, der Theologie; die\npietistisch gesinnten jungen Theologen kamelisirten und kamen im Studentenleben nicht\nviel in Betracht. Lobeck kämpfte in seinem philologischen Seminar mit Erfolg\ngegen alle romantischen Einfälle, die seiner Wissenschaft zu nahe traten; er galt\nals Republikaner, da er ohnehin mehr in Rom und Athen, als in Königsberg\nlebte. Schubert, Voigt, Drumann waren entschieden Preußisch gesinnt; Schu¬\nbert stellte mit einem diplomatisch feinen Lächeln die Spießbürgerlichkeit der alten\nRepubliken dem complicirten modernen Staatsleben gegenüber ins Licht; er liebte\nes, mit Personen von Stande umzugehn und ironisirte bei seinen Schülern ein\netwa hervortretendes jugendliches Interesse an der französischen Revolution, aber\ner war dabei aufgeklärt und gegen allen Obscurantismus in der Kirche wie im\nStaate. Mit den staatsökonomischen Kollegien richtete er nicht viel aus, man\nlernte ebendie Hefte, soviel zum Examen nöthig war und dachte conservativ ge¬\nnug, sich am Gegebenen genügen zu lassen. Voigt stand mit dem Oberpräsiden¬\nten in näherer Verbindung; er war schon seiner amtlichen Beschäftigung nach,\nseinen archivarischen Arbeiten nämlich, Royalist; Drumann war es im Princip, er\nsah die ganze römische Geschichte bis auf Cäsar nur als Vorbereitung an für die\nmonarchische Verfassung, die später eintrat. Auch der Jurist Simson, der ge¬\ngenwärtig in Frankfurt ist und der durch einen fließenden Vortrag imponirte, war\nloyal; er wußte sich etwas darauf, im Tribunal zu sitzen und so dem Staate nä¬\nher anzugehören.\n\nAuf legitime Weise wurde also der oppositionelle Sinn der Studirenden\nnicht genährt. Dagegen blieb eine stille Opposition. In keinem Corps wurde\nder präjudicielle Vers des Landesvaters gemacht. Bei den rechten Studenten galt\nes für schlechten Ton, sich mit Politik abzugeben oder gar die Zeitung zu lesen;\naber hin und wieder verirrte sich ein alter Bursch, der von fremden Universitäten\nrelegirt war, nach Königsberg und sah mit dem ganzen Bewußtsein eines verfolgten\nPatrioten auf die gewöhnlichen Sterblichen herab. Je stoffloser dieser Radicalismus\nwar, desto gründlicher verachtete er das Bestehende. Man las seinen Thiers und Mignet\nund schwärmte demnach für Mirabeau und Danton — bis zu Robespierre ver¬\nstieg man sich noch nicht; man war Republikaner, verachtete die Deutschen und\nlas in der Allgemeinen Zeitung nur die Französischen und Englischen Artikel; man\ntrieb nur grande politique und combinirte eine beliebige Niederlage Esparteros mit\neinem Tscherkessischen Krawall, und glücklich in diesem Bewußtsein großer Princi¬\npien hielt man sich des politischen Details für überhoben und trieb die sonstigen\n\n\n56*\f\n"
                               :right "439\nalljährlich wurde eine Festrede zu Ehren Kant's gehalten. Rosenkranz liebte es,\njunge Talente um sich zu sammeln; damals wurde es Sitte, sich lyrisch gehn zu\nlassen, zuerst in sentimentalen Trinkliedern, doch spielte bald die Freiheit im All¬\ngemeinen und der aufgeklärte Patriotismus hinein, und es fehlte weder die Heiuesche\nZerrissenheit noch die Grün'sche Reflexionspoefie. Die Mehrzahl der Professoren\nhielt auf rationalistischeAuffassung der Geschichte, des Rechts, der Theologie; die\npictistisch gesinnten jnngen Theologen kamelisirten nnd kamen im Studentenlcben nicht\nviel in Betracht. Lobeck kämpfte in seinem philologischenSeminar mit Erfolg\ngegen alle romantischenEinfälle, die seiner Wissenschaft zu nahe traten; er galt\nals Republikaner, da er ohnehin mehr in Rom und Athcu, als in Königsberg\nlebte. Sch ubert, V oigt, Druman n waren entschieden Preußisch gesinut; Schu¬\nbert stellte mit einem diplomatischfeinen Lächeln die Spießbürgerlichkcit der alten\nRepubliken dem complicirten modernen Staatsleben gegenüber ins Licht; er liebte\nes, mit Personen von Stande umzugehn und ironisirte bei seinen Schülern ein\netwa hervortretendes jugendliches Interesse an der französischen Revolution, aber\ner war dabei aufgeklärt und gegen allen Obscurantismns in der Kirche wie im\nStaate. Mit den staatsvkonvmischenKollegien richtete er nicht viel aus, man\nlernte cbeu die Hefte, soviel zum Examen nöthig war uud dachte couservativ ge¬\nnug, sich am Gegcbeucn genügen zn lassen. Voigt stand mit dem Oberpräsiden¬\nten in näherer Verbindung; er war schon seiner amtlichen Beschäftigung nach,\nseinen archivarischen Arbeiten nämlich, Royalist; Drnmann war es im Princip, er\nsah die ganze römische Geschichte bis auf Cäsar nur als Vorbereitung an für die\nmonarchische Verfassung, die später eintrat. Auch der Jurist Simsvn, der ge¬\ngenwärtig in Frankfurt ist und der durch eiucu fließenden Vortrag imponirte, war\nloyal; er wußte sich etwas darauf, im Tribunal zu sitzen und so dem Staate nä¬\nher anzugehören.\nAns legitime Weise wurde also der oppositionelle Sinn der Studirenden\nnicht genährt. Dagegen blieb eine stille Opposition. In keinem Corps wurde\nder präjudiciclle Vers des Landesvaters gemacht. Bei den rechten Studenten galt\nes für schleckten Ton, sich mit Politik abzugeben oder gar die Zeitung zn lesen;\naber hin und wieder verirrte sich ein alter Bursch, der von fremden Universitäten\nrelegirt war, nach Königsberg und sah mit dem ganzen Bewußtsein eines verfolgten\nPatnoten auf die gewöhnlichen Sterblichen bcrab. Je stosslvser dieser Nadicalismus\nwar, desto gründlicher verachtete er das Bestehende.Mm las seine» Thiers nnd Mignct\nnnd schwärmte demnach für Mirabeau nnd Danton — bis zn Nvbe^pierre ver¬\nwieg man sich noch nicht; man war Republikaner, verachtete die Dcutschcu und\nlas in der Allgemeinen Zeitung nur die Französischen uud Englische» Artikel; man\ntrieb nur Ai-itinlo >wI>U,M' nnd combinirte eine beliebige Niederlage Esparteros mit\neinem TscherkessischeuKrawall, uud glücklich iu diesem Bewnßtsein großer Princi¬\npien hielt man sich des politischen Details für überhoben und trieb die sonstigen\n56*\n\n"
                               :error-codes '([[1 1] [324 324]] [[1 1] [377 377]] [[5 8] [437 437]] [[1 1] [498 497]] [[1 1] [519 518]] [[1 1] [548 547]] [[1 1] [571 570]] [[5 8] [639 638]] [[5 8] [682 680]] [[1 1] [792 789]] [[1 1] [793 790]] [[8 5] [824 821]] [[8 5] [832 830]] [[8 5] [844 843]] [[1 1] [879 879]] [[5 8] [924 924]] [[1 1] [960 959]] [[1 1] [1264 1263]] [[1 1] [1310 1309]] [[1 1] [1314 1313]] [[5 8] [1322 1321]] [[1 1] [1372 1370]] [[1 1] [1375 1373]] [[8 5] [1376 1374]] [[1 1] [1417 1416]] [[1 1] [1429 1428]] [[1 1] [1459 1458]] [[1 1] [1462 1461]] [[1 1] [1463 1462]] [[1 1] [1475 1474]] [[1 1] [1648 1647]] [[1 1] [1821 1820]] [[1 1] [1876 1875]] [[1 1] [1877 1876]] [[1 1] [1878 1877]] [[4 8] [2010 2009]] [[1 1] [2012 2010]] [[1 1] [2013 2011]] [[1 1] [2169 2167]] [[1 1] [2249 2247]] [[1 1] [2308 2306]] [[7 1] [2483 2481] [2484 2482]] [[1 1] [2523 2520]] [[1 1] [2524 2521]] [[1 1] [2536 2533]] [[1 1] [2537 2534]] [[1 1] [2539 2536]] [[1 1] [2551 2548]] [[5 8] [2616 2613]] [[7 1] [2618 2614] [2619 2615]] [[1 4] [2630 2625]] [[1 1] [2639 2634]] [[1 1] [2647 2642]] [[1 1] [2650 2645]] [[1 1] [2685 2680]] [[1 1] [2703 2698]] [[1 1] [2705 2700]] [[1 1] [2706 2701]] [[1 4] [2709 2704]] [[7 1] [2722 2717] [2723 2718]] [[1 1] [2787 2781]] [[1 1] [2793 2787]] [[1 1] [2794 2788]] [[1 1] [2854 2848]] [[1 4] [2866 2860]] [[1 1] [2891 2885]] [[1 1] [2892 2886]] [[1 7] [2893 2887] [2894 2888]] [[8 1] [2894 2889]] [[8 1] [2894 2890]] [[1 1] [2895 2892]] [[1 1] [2896 2893]] [[1 4] [2898 2895]] [[7 1] [2899 2896] [2900 2897]] [[1 4] [2900 2898]] [[1 1] [2901 2899]] [[1 2] [2902 2900]] [[1 1] [2903 2901]] [[1 4] [2904 2902]] [[1 5] [2905 2903]] [[1 1] [2908 2904]] [[1 1] [2985 2981]] [[5 8] [2986 2982]] [[1 1] [2997 2992]] [[1 1] [3011 3006]] [[1 1] [3023 3018]] [[4 8] [3128 3123]] [[4 8] [3129 3123]] [[4 4] [3133 3126]])}
                              {:left "Mammut" :right
                               "rnaiiiiiiiit"
                               :error-codes '([[1 7] [0 0] [1 1]] [[1 7] [2 3] [3 5]] [[1 7] [3 6] [4 8]] [[1 7] [4 9] [5 10]])}
                              ]}))

(defn highlight-view [highlight owner]
  (reify
    om/IRender
    (render [this]
         (if (string? highlight)
           (do #_(prn "string " (dom/span nil highlight))
           (dom/span nil (.replace highlight #"\r?\n" "\\n")))
           (dom/span #js {:style #js {:backgroundColor (:color highlight)}
                          :id (:id highlight)}
                     (.replace (:text highlight) #"\r?\n" "\\n"))))))


(defn goto-and-mark [error-code i]
  (let [idl (str i "-" error-code "-l")
        idr (str i "-" error-code "-r")]
    (.scrollIntoView (.getElementById js/document idl) false)
    (d/add-class! (d/by-id idl) "boxed")
    (d/add-class! (d/by-id idr) "boxed")
    #_(prn "now the style of l is " (aget (.-style (.getElementById js/document idl)) "border"))
    (js/setTimeout (fn []
                      (prn "unsetting")
                       (d/remove-class! (d/by-id idl) "boxed")
                       (d/remove-class! (d/by-id idr) "boxed")) 2000)))

(defn error-code-link-view [[error-code left right i] owner]
  (reify
    om/IRender
    (render [this]
       #_(prn "left right " (count left) (count right))
      (let [[hl hr] (build-highlight error-code left right i)
            #_#__ (prn "hr " hr "hl " hl "texts " (:text hl) (:text hr))]
        (dom/a #js {:onClick (fn [& args] (goto-and-mark error-code i))}
               (str (first error-code) ":" (:text hl) "->" (:text hr)))))))



(defn table-view [[error-codes left right i] owner]
  (reify
    om/IRender
    (render[this]
      #_(prn "render-table-view " error-codes)
      (let [categories (group-by get-type error-codes)]
      (dom/div #js {:className "table-div"}
         (dom/table #js {:className "table" ;:border "1"
                         :id (str "table-" i)}
            (apply dom/tbody nil
              (dom/tr nil
                (dom/th nil "Fehlerart") (dom/th nil "Fehler") (dom/th nil "Anzahl"))
               (for [[k codes] categories]
                 (dom/tr nil
                   (dom/td nil (name k))
                   (apply dom/td nil (om/build-all error-code-link-view
                                 (map (fn [a] [a left right i]) codes)))
                   (dom/td nil (str (count codes))))))))))))


(defn page-summary-view [[{:keys [error-codes left right name]} i] owner]
  (reify
    om/IRender
    (render [this]
      (dom/div #js {:className "summary-div"}
               (dom/p nil name)
               (om/build table-view [error-codes left right i])))))


(defn page-view [[page i] owner]
  (reify
    om/IRender
    (render [this]
      (let [[hl hr] (build-highlights (:error-codes page) (:left page) (:right page) i)
            _ (prn "hl hr " hl hr)]
       (dom/div #js {:className "wrap"}
         (om/build page-summary-view [page i])
         (apply dom/div #js {:className "left" :id (str "left-" i)}
                   (om/build-all highlight-view hl ))
         (apply dom/div #js {:className "right" :id (str "right-" i)}
                    (om/build-all highlight-view hr)))))))



(defn pages-view [pages owner]
  (reify
    om/IRender
    (render [this]
            (prn "hi there")
      (apply dom/div nil
        (om/build-all page-view (map (fn [a b] [a b]) pages (range)))))))

(defn load-or-remove-page [load-chan page id]
  (put! load-chan [page id]))

(defn handle-change [load-chan selected-idx page i]
  (prn "handle-change" load-chan selected-idx page i)
  (if-let [p (some #{page} (map :name (get @app-state :pages)))]
    (swap! app-state update-in [:pages] #(remove (comp #{page} :name) %))
    (GET (str "/get-site/" i "?bd=" (:base-directory  @app-state))
         {:handler (fn [data]
                     (swap! app-state update-in [:pages] #(sort-by :name (concat % [(assoc data :name page)]))))})))

(defn page-list-view [available-pages owner]
  (reify
    om/IInitState
    (init-state [_]
      {:load-chan (chan)
       :selected-idx []})
    om/IRenderState
    (render-state [this {:keys [load-chan selected-idx]}]
      (prn "available-pages " available-pages load-chan)
      (apply dom/ul nil
             (for [[page i] (map (fn [a b] [a b]) available-pages (range))]
               (dom/li nil (dom/input
                            #js {:type "checkbox"
                                  :onClick
                                 (fn []
                                   (do (prn "called " load-chan page i)
                                     (handle-change load-chan selected-idx
                                                  page i)))} page)))))))

(defn get-page-list [app-state]
  (prn "get-page-list " (:base-directory @app-state))
  (GET (str "/page-list?bd=" (:base-directory @app-state))
    {:handler (fn [data]
                (prn "get-page-list callback called " data)
                (swap! app-state assoc :available-pages data))}))

(defn page-select-view [app owner]
  (reify
    om/IRender
    (render [this]
      (if-not (seq (:available-pages app))
        (dom/div nil
                 (dom/input #js {:type "text"
                                 :size "300"
                                 :id "base-directory-input"
                                 :name "base-directory-input"
                                 :onClick
                                 (fn []
                                   (do (prn "file-clicked")))})
                 (dom/input #js {:value "set base directory"
                                 :type "button"
                                  :onClick (fn [] (prn "button clicked")
                                             (let [bd-input (d/by-id "base-directory-input")]
                                               (swap! app-state assoc :base-directory
                                                      (d/value bd-input))
                                               (get-page-list app-state)))}))
        (dom/div nil
                 (dom/h3 nil (str (:base-directory app)))
                 (dom/div nil "Lade Seiten: ")
                 (om/build page-list-view (:available-pages app)))))))
(defn init []
  #_(GET "/page-list" {:handler (fn [data] (swap! app-state assoc :available-pages data))})
  (om/root
   (fn [app owner]
     ;;(om/build page-view (first (:pages app))))
     (dom/div nil
       (om/build page-select-view app)
       (dom/div nil (om/build pages-view (:pages app)))))
   ;;  (dom/div #js {:id "ID"} "hi")
     ;;(om/build (fn [a o] (reify om/IRender (render [this] (dom/span nil "hi")))) nil))
   app-state
   {:target (.getElementById js/document "page-container")}))

(set! (.-onload js/window)
      #(do
;;         (repl-connect)
         ;;        (visualize-errors)
         (prn "hi")
         (init)
         ))









