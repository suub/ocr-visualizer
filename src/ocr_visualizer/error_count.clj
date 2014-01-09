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
(set-current-implementation :ndarray)

(def counter (atom {:i 0 :j 0}))


(defn backtrace [d i j acc]
  (cond
   (and (> i 0) (= (inc (mget d (dec i) j)) (mget d i j)))
   (recur d (dec i) j (assoc acc :deletions (inc (:deletions acc))))
   (and (> j 0) (= (inc (mget d i (dec j))) (mget d i j)))
   (recur d i (dec j) (assoc acc :insertions (inc (:insertions acc))))
   (and (> i 0) (> j 0) (= (inc (mget d (dec i) (dec j))) (mget d i j)))
   (recur d (dec i) (dec j) (assoc acc :substitutions(inc (:substitutions acc))))
   (and (> i 0) (> j 0) (= (mget d (dec i) (dec j)) (mget d i j)))
   (recur d (dec i) (dec j) acc)
   :else acc))


(defn lev [str1 str2]
  (let [mat (make-array Integer/TYPE (inc (count str1)) (inc (count str2)))]
    (aset mat 0 0 (int 0))
    (dotimes [i (count str1)]
      (aset mat (inc i) 0 (int (inc i))))
    (dotimes [j (count str2)]
      (aset mat 0 (inc j) (int (inc j))))
    (dotimes [dj (count str2)]
      (dotimes [di (count str1)]
        (let [j (inc dj) i (inc di)]
          (aset mat i j
                (int (cond
                      (= (.charAt ^String str1 di) (.charAt ^String str2 dj))
                      (aget mat di dj)
                      :else
                      (min (inc (aget mat di j)) (inc (aget mat i dj))
                           (inc (aget mat di dj)))))))))
                          
    mat))



(defn edits [a b]
  (let [d (lev a b)]
    (backtrace d (count a) (count b) {:insertions 0 :deletions 0
                                      :substitutions 0
                                      :distance (aget d (count a) (count b))})))
