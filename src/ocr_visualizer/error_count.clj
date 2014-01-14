(ns ocr-visualizer.error-count
  (:use [clojure.core.matrix]))
(set! *warn-on-reflection* true)

;;test für replacement ... wenn zeichen nicht übereinanderstimmen
;;test für mehrere zu einem  ---
;;der fehler soll aufkommen, wenn man , in dem man mehrere zeichen zu einem macht, ein besseres ergebnis (weniger ersetzungen etc) erzielt als ohne
;;test für ein zu mehreren ----
;;der fehler soll aufkommen, wenn man durch zerlegung eines zeichens weniger
;;fehler macht als sonst
;;geht ja eigentlich auch über levenshtein wenn man aufzeichnet was gemacht wurde und substitution plus deletion von nacheinanderstehenden zusammenmerged....

(defmacro dbg [body]
  `(let [x# ~body]
     (println "dbg:" '~body "=" x#)
     x#))

(defn update-deletion [lev]
  ;;the first char of seq2 got deleted
  (assoc lev
          :distance (inc (:distance lev))
          :deletion (inc (:deletions lev))))

(defn update-insertion [lev]
  (assoc lev
    :distance (inc (:distance lev))
    :insertions (inc (:insertions lev))
    ))

(defn update-substitution [lev]
  (assoc lev
    :distance (inc (:distance lev))
    :substitutions (inc (:substitutions lev))))
    
(defn levenshtein-distance
  "Calculates the edit-distance between two sequences"
  [seq1 seq2]
  (cond
   (empty? seq1) {:distance (count seq2)
                  :deletions (count seq2)
                  :insertions 0
                  :substitutions 0}
   (empty? seq2) {:distance (count seq1)
                  :deletions 0
                  :insertions (count seq2)
                  :substitutions 0}
   :else (let
             [levdel (update-deletion (#'levenshtein-distance (rest seq1) seq2)
                                seq1 seq2)
              levins (update-insertion (#'levenshtein-distance seq1 (rest seq2))
                                seq1 seq2)
              levsubs (update-substitution (#'levenshtein-distance (rest seq1) (rest seq2)) seq1 seq2)]
           (first (sort #(< (:distance %1) (:distance %2))
                        [levdel levins levsubs])))))


(defn levenshtein-distance
  "Calculates the edit-distance between two sequences"
  [seq1 seq2]
  (cond
   (empty? seq1) (count seq2)
   (empty? seq2) (count seq1)
   :else (min
          (+ (if (= (first seq1) (first seq2)) 0 1)
             (#'levenshtein-distance (rest seq1) (rest seq2))) 
          (inc (#'levenshtein-distance (rest seq1) seq2))      
          (inc (#'levenshtein-distance seq1 (rest seq2))))))


;; We cast a spell more powerful than either:
(def levenshtein-distance (memoize levenshtein-distance))

(levenshtein-distance "avada kedavra" "abracadabra") ; ok, it was seven.

(set-current-implementation :ndarray)

(defn dlev [str1 str2]
  (let [mat (new-matrix (inc (count str1)) (inc (count str2)))]
    (mset! mat 0 0 {:distance 0 :deletions 0 :insertions 0 :substitutions 0})
    (dotimes [i (count str1)]
      (mset! mat (inc i) 0 {:distance (inc i)
                            :deletions (inc i)
                            :insertions 0
                            :substitutions 0}))
    (dotimes [j (count str2)]
      (mset! mat 0 (inc j) {:distance (inc j)
                            :deletions 0
                            :insertions (inc j)
                            :substitutions 0}))
    (dotimes [dj (count str2)]
      (dotimes [di (count str1)]
        (let [j (inc dj) i (inc di)]
          (mset! mat i j
                 (cond
                  (= (.charAt ^String str1 di) (.charAt ^String str2 dj))
                  (mget mat di dj)
                  :else
                  (first (sort #(< (:distance %1) (:distance %2))
                               [(update-deletion  (mget mat di j))
                                (update-insertion  (mget mat i dj))
                                (update-substitution (mget mat di dj))])))))))
    (mget mat (count str1) (count str2))))

(defn dlev [str1 str2]
  (let [mat (new-matrix (inc (count str1)) (inc (count str2)))]
    (mset! mat 0 0 {:distance 0 :deletions 0 :insertions 0 :substitutions 0})
    (dotimes [i (count str1)]
      (mset! mat (inc i) 0 {:distance (inc i)
                            :deletions (inc i)
                            :insertions 0
                            :substitutions 0}))
    (dotimes [j (count str2)]
      (mset! mat 0 (inc j) {:distance (inc j)
                            :deletions 0
                            :insertions (inc j)
                            :substitutions 0}))
    (dotimes [dj (count str2)]
      (dotimes [di (count str1)]
        (let [j (inc dj) i (inc di)]
          (mset! mat i j
                 (cond
                  (= (.charAt ^String str1 di) (.charAt ^String str2 dj))
                  (mget mat di dj)
                  :else
                  (first (sort #(< (:distance %1) (:distance %2))
                               [(update-deletion  (mget mat di j))
                                (update-insertion  (mget mat i dj))
                                (update-substitution (mget mat di dj))])))))))
    (mget mat (count str1) (count str2))))

(def counter (atom {:i 0 :j 0}))

(defn lev [str1 str2]
  (let [mat (new-matrix (inc (count str1)) (inc (count str2)))]
    (prn "start lev")
    (mset! mat 0 0 0)
    (dotimes [i (count str1)]
      (mset! mat (inc i) 0 (inc i)))
    (dotimes [j (count str2)]
      (mset! mat 0 (inc j) (inc j)))
    (dotimes [dj (count str2)]
      (dotimes [di (count str1)]
        (let [j (inc dj) i (inc di)
              _ (reset! counter {:i i :j j})]
          (mset! mat i j
                 (cond
                  (= (.charAt ^String str1 di) (.charAt ^String str2 dj))
                  (mget mat di dj)
                  :else
                  (min (inc (mget mat di j)) (inc (mget mat i dj))
                       (inc (mget mat di dj))))))))
    mat))

(defn backtrace [d i j acc]
  (cond
   (and (> i 0) (= (inc (mget d (dec i) j)) (mget d i j)))
   (recur d (dec i) j (assoc acc :deletions (cons [i j] (:deletions acc))))
   (and (> j 0) (= (inc (mget d i (dec j))) (mget d i j)))
   (recur d i (dec j) (assoc acc :insertions (cons [i j] (:insertions acc))))
   (and (> i 0) (> j 0) (= (inc (mget d (dec i) (dec j))) (mget d i j)))
   (recur d (dec i) (dec j) (assoc acc :substitutions (cons [i j] (:substitutions acc))))
   (and (> i 0) (> j 0) (= (mget d (dec i) (dec j)) (mget d i j)))
   (recur d (dec i) (dec j) acc)
   :else acc))


(defn lev2 [str1 str2]
  (let [mat (new-matrix (inc (count str1)) (inc (count str2)))
        akt (new-matrix 1 1)]
    (mset! mat 0 0 0)
    (dotimes [i (count str1)]
      (mset! mat (inc i) 0 (inc i)))
    (dotimes [j (count str2)]
      (mset! mat 0 (inc j) (inc j)))
    (dotimes [dj (count str2)]
      (dotimes [di (count str1)]
        (let [j (inc dj) i (inc di)]
          (mset! mat i j
                 (cond
                  (= (.charAt ^String str1 di) (.charAt ^String str2 dj))
                  (mget mat di dj)
                  :else
                  (min (inc (mget mat di j)) (inc (mget mat i dj))
                       (inc (mget mat di dj))))))))
    mat))

#_(def a (slurp "data/186498 meld.txt"))
#_(def b (slurp "data/186498 tess.txt"))

#_(def a (slurp "data/all meld.txt"))
#_(def b (slurp "data/all tess.txt"))
#_(def m (lev a b))
#_(def res (backtrace m (count a) (count b) {:insertions 0 :substitutions 0 :deletions 0}))

(defn edits [a b]
  (let [d (lev a b)]
    (backtrace d (count a) (count b) {:insertions '() :deletions '()
                                      :substitutions '()
                                      :distance (mget d (count a) (count b))})))