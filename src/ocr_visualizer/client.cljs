(ns ocr-visualizer.client
    (:require [enfocus.core :as ef]
              [enfocus.effects :as effects]
              [enfocus.events :as events]
              [clojure.browser.repl :as repl])
    (:use-macros [enfocus.macros :only [deftemplate defsnippet defaction]]))

(declare add-fruit)

;;************************************************
;; Dev stuff
;;************************************************
(def dev-mode true)

(defn repl-connect [] 
 (when dev-mode
   (repl/connect "http://localhost:9000/repl")))

;;************************************************
;; Retrieving data from dom
;;************************************************

(defn by-id [id]
  (.getElementById js/document id))

;; if from is sent one element it returns the value
(defn get-name []
 (ef/from (by-id "yourname") (ef/get-prop :value)))

;; if from is passed a set lookups it returns 
;; a map {:fruit "apple" :quanity "10" }
(defn get-fruit-vals []
 (ef/from js/document
   :fruit "#fruit" (ef/get-prop :value)
   :quanity "#quantity" (ef/get-prop :value)))


;;************************************************
;; snippets and templates
;;************************************************

;; we can use enlive based selects 
;; along side string based selectors
(defsnippet home-snip :compiled "public/index.html" [:#stage] []) 
  
(deftemplate welcome-temp :compiled "public/templates/welcome.html" [name]
   "#name" (ef/content name)
   "#time" (ef/content (.toISOString (js/Date.))))

;note selectors can be vectors or bare strings
(defsnippet fruit-snip :compiled "public/templates/welcome.html" 
  ["tbody > *:first-child"] [fruit quantity]
  ["tr > *:first-child"] (ef/content fruit)
  ["tr > *:last-child"]  (ef/content quantity)) 
   

;;************************************************
;; actions/navigation
;;************************************************

(defaction add-fruit [data]
  "tbody" (ef/append (fruit-snip (:fruit data) (:quanity data))))  

(defaction welcome []
  "#stage" (ef/substitute (welcome-temp (get-name)))
  "#home-btn" (ef/remove-class "active")
  "#fruit-btn" (events/listen :click
                              #(add-fruit (get-fruit-vals))))

(defaction home []
  "#stage" (ef/substitute (home-snip))
  "#home-btn" (ef/add-class "active")
  "#welcome-btn" (events/listen :click welcome))
                
(defaction init []
  "#home-btn" (events/listen :click home))
;;************************************************
;; onload
;;************************************************

(set! (.-onload js/window)
      #(do
         (repl-connect)
         (init)
         (home)))
