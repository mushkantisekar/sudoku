(ns sudoku.engine
  (:require [clojure.set :as st]))

(def possible-vals #{1 2 3 4 5 6 7 8 9})
(def solved-sudoku
  [[1 2 3 4 5 6 7 8 9]
   [4 5 6 7 8 9 1 2 3]
   [7 8 9 1 2 3 4 5 6]
   [2 3 4 5 6 7 8 9 1]
   [5 6 7 8 9 1 2 3 4]
   [8 9 1 2 3 4 5 6 7]
   [3 4 5 6 7 8 9 1 2]
   [6 7 8 9 1 2 3 4 5]
   [9 1 2 3 4 5 6 7 8]])

(def empty-sudoku (vec (for [i (range 9)] (vec (for [j (range 9)] nil)))))
(defn possible-val? [n] (contains? possible-vals n))
(defn map-sudoku [f sk] (vec (map #(vec (map f %)) sk)))
(defn sudoku->list [sk] (reduce concat sk))
(defn reduce-sudoku [f val sk] (reduce f val (sudoku->list sk)))
(defn map-indexed-sudoku [f sk]
  (vec (map-indexed
        (fn [r rv]
          (vec (map-indexed
                (fn [c v] (f r c v))
                rv)))
        sk)))
(defn get-cell [sk r c] (-> sk (get r) (get c)))
(defn set-cell [sk r c v] (assoc-in sk [r c] v))
(defn row [sk n] (get sk n))
(defn col [sk n] (vec (for [i (range 9)] (get-cell sk i n))))
(defn sqr [sk r c]
  (let [ir (* 3 (quot r 3))
        ic (* 3 (quot c 3))]
    (vec (for [i (range 3) j (range 3)]
           (get-cell sk (+ ir i) (+ ic j))))))

(defn options-cell [sk r c v]
  (let* [sk (set-cell sk r c nil)
         vals (into #{} (st/union (row sk r)
                                  (col sk c)
                                  (sqr sk r c)))
         opts (st/difference possible-vals vals)]
    (if-not (possible-val? v) ; nil or incorrect
      opts
      (if (contains? opts v)
        #{v}
        #{}))))

(defn options [sk]
  (map-indexed-sudoku (fn [r c v] (options-cell sk r c v)) sk))

(defn incorrect? [sk] (reduce-sudoku #(or %1 (= 0 (count %2))) false (options sk)))
(defn solved? [sk] (reduce-sudoku #(and %1 (= 1 (count %2))) true (options sk)))

(defn complete-sudoku-once [sk]
  (let [sk-opts (options sk)]
    (map-indexed-sudoku
     (fn [r c v]
       (let [cell-opts (get-cell sk-opts r c)]
         (if (= 1 (count cell-opts))
           (first cell-opts)
           v)))
     sk)))

(defn complete-sudoku [sk]
  (loop [orig sk]
    (let [updt (complete-sudoku-once orig)]
      (if (= orig updt)
        orig
        (recur updt)))))

(defn solve-sudoku [sk]
  (cond
    (incorrect? sk)  nil
    (solved? sk)     sk
    :else            'FIXME
    ))
