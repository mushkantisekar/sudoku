(ns sudoku.engine
  (:require [clojure.set :as st]))

(def possible-vals #{1 2 3 4 5 6 7 8 9})

(defn possible-val? [n] (contains? possible-vals n))
(defn reduce-sudoku [f val sk] (reduce f val (reduce concat sk)))
(defn map-indexed-sudoku [f sk]
  (vec (map-indexed (fn [r rv] (vec (map-indexed (fn [c v] (f r c v)) rv))) sk)))

(defn get-cell [sk r c] (-> sk (get r) (get c)))
(defn set-cell [sk r c v] (assoc-in sk [r c] v))
(defn row [sk n] (get sk n))
(defn col [sk n] (vec (for [i (range 9)] (get-cell sk i n))))
(defn sqr [sk r c]
  (let [ir (* 3 (quot r 3))
        ic (* 3 (quot c 3))]
    (vec (for [i (range 3) j (range 3)] (get-cell sk (+ ir i) (+ ic j))))))

(defn options-cell [sk r c v]
  (let* [sk (set-cell sk r c nil)
         vals (into #{} (st/union (row sk r)
                                  (col sk c)
                                  (sqr sk r c)))
         opts (st/difference possible-vals vals)]
    (if-not (possible-val? v) opts (if (contains? opts v) #{v} #{}))))

(defn options [sk] (map-indexed-sudoku (fn [r c v] (options-cell sk r c v)) sk))
(defn solved? [sk] (reduce-sudoku #(and %1 (= 1 (count %2))) true (options sk)))
(defn invalid? [sk]
  (->> (options sk)
       (map-indexed-sudoku
        (fn [r c v] (or (contains? v (get-cell sk r c))
                        (not (possible-val? (get-cell sk r c))))))
       (flatten)
       (reduce #(and %1 %2) true)
       not))

(defn complete-sudoku-once [sk]
  (let [sk-opts (options sk)]
    (map-indexed-sudoku
     (fn [r c v] (let [cell-opts (get-cell sk-opts r c)]
                   (if (= 1 (count cell-opts)) (first cell-opts) v)))
     sk)))

(defn complete-sudoku [sk]
  (if (invalid? sk)
    nil
    (loop [orig sk]
      (let [updt (complete-sudoku-once orig)]
        (if (= orig updt) orig (recur updt))))))

(declare -solve-sudoku)

(defn solution-seq [sol-tree]
  (filter vector? (tree-seq #(not (vector? %)) identity sol-tree)))

(defn try-opts-at [sk r c opts random?]
  (->> (lazy-seq opts)
       (map #(set-cell sk r c %))
       (map #(-solve-sudoku % random?))
       (filter #(not (or (empty? %) (nil? %))))))

(defn solve-open-sudoku [sk random?]
  (let [opts      (options sk)
        selec-fn  (if random?
                    (fn [coll] (if (empty? coll) nil (rand-nth coll)))
                    first)]
    (->> opts
         (map-indexed-sudoku (fn [r c v] [r c (count v)])) ; Take only cells with
         (reduce concat)                                   ; multiple options
         (filter #(> (% 2) 1))                             ; available
         selec-fn
         ((fn [[r c _]] (try-opts-at sk r c (get-cell opts r c) random?))))))

(defn -solve-sudoku [sk random?]
  (let [sk (complete-sudoku sk)]
    (if (solved? sk)
      (list sk)
      (solve-open-sudoku sk random?))))

(defn solve-sudoku
  ([sk]         (solve-sudoku sk false))
  ([sk random?] (solution-seq (-solve-sudoku sk random?))))

(defn solvable? [sk] (-> sk (solve-sudoku) (first) nil? not))
(defn uniq-solution? [sk]
  (let [sols (solve-sudoku sk)
        sol1 (first sols)
        sol2 (second sols)]
    (and (not (nil? sol1))
         (nil? sol2)))
  )

(defn remove-random-element [sk]
  (->> sk
       (map-indexed-sudoku (fn [r c v] [r c v (rand)]))
       (reduce concat)
       (filter #(possible-val? (nth % 2)))
       (sort-by #(nth % 3))
       (map #(set-cell sk (nth % 0) (nth % 1) nil))
       (filter uniq-solution?)
       (first)))

(defn remove-elements [sk max]
  (loop [curr sk
         left max]
    (if (<= left 0)
      curr
      (let [next (remove-random-element curr)]
        (println "Iteration\n" next "\n\n")
        (if (nil? next)
          curr
          (recur next (- left 1)))))))

(let [_ nil]
  (def sk-empty
    [[_ _ _ _ _ _ _ _ _]
     [_ _ _ _ _ _ _ _ _]
     [_ _ _ _ _ _ _ _ _]
     [_ _ _ _ _ _ _ _ _]
     [_ _ _ _ _ _ _ _ _]
     [_ _ _ _ _ _ _ _ _]
     [_ _ _ _ _ _ _ _ _]
     [_ _ _ _ _ _ _ _ _]
     [_ _ _ _ _ _ _ _ _]])

  (def sk-solved
    [[1 2 3 4 5 6 7 8 9]
     [4 5 6 7 8 9 1 2 3]
     [7 8 9 1 2 3 4 5 6]
     [2 3 4 5 6 7 8 9 1]
     [5 6 7 8 9 1 2 3 4]
     [8 9 1 2 3 4 5 6 7]
     [3 4 5 6 7 8 9 1 2]
     [6 7 8 9 1 2 3 4 5]
     [9 1 2 3 4 5 6 7 8]])

  (def sk-invalid
    [[2 2 3 4 5 6 7 8 9]
     [4 5 6 7 8 9 1 2 3]
     [7 8 9 1 2 3 4 5 6]
     [2 3 4 5 6 7 8 9 1]
     [5 6 7 8 9 1 2 3 4]
     [8 9 1 2 3 4 5 6 7]
     [3 4 5 6 7 8 9 1 2]
     [6 7 8 9 1 2 3 4 5]
     [9 1 2 3 4 5 6 7 8]])

  (def sk-directly-solvable
    [[1 _ 3 4 _ 6 7 _ _]
     [4 _ 6 _ 8 _ _ _ 3]
     [_ 8 9 _ 2 3 _ 5 _]
     [2 _ 4 _ _ 7 _ _ _]
     [_ _ 7 _ 9 _ _ 3 _]
     [8 _ _ 2 _ 4 _ 6 _]
     [_ _ 5 _ 7 _ _ _ 2]
     [6 _ 8 _ _ 2 _ 4 _]
     [9 _ 2 _ 4 _ 6 _ 8]])

  (def sk-open-single-solution
    [[1 _ _ 4 _ 6 7 _ _]
     [4 _ 6 _ 8 _ _ _ 3]
     [_ 8 _ _ 2 _ _ 5 _]
     [2 _ 4 _ _ 7 _ _ _]
     [_ _ 7 _ 9 _ _ 3 _]
     [8 _ _ 2 _ 4 _ 6 _]
     [_ _ _ _ 7 _ _ _ 2]
     [6 _ 8 _ _ 2 _ 4 _]
     [9 _ _ _ 4 _ 6 _ 8]])

  (def sk-open-multiple-solutions
    [[1 _ _ 4 _ _ 7 _ _]
     [4 _ 6 _ 8 _ _ _ 3]
     [_ 8 _ _ 2 _ _ 5 _]
     [2 _ 4 _ _ 7 _ _ _]
     [_ _ 7 _ _ _ _ 3 _]
     [8 _ _ 2 _ 4 _ 6 _]
     [_ _ _ _ 7 _ _ _ 2]
     [6 _ 8 _ _ 2 _ 4 _]
     [9 _ _ _ 4 _ 6 _ 8]])

  (def sk-unsolvable
    [[1 _ _ 4 _ _ 7 _ _]
     [4 _ 6 _ 8 _ _ _ 3]
     [_ 8 _ _ 2 _ _ 5 _]
     [2 _ 4 _ _ 7 _ _ _]
     [_ _ 7 _ _ _ _ 3 _]
     [8 _ _ 2 _ 4 _ 6 _]
     [_ _ _ _ 7 _ _ _ 2]
     [6 _ 8 _ _ 2 _ 5 _]
     [9 _ _ _ 4 _ 6 _ 8]]))

(defn generate-random-sudoku []
  (first (solve-sudoku sk-empty true)))


;; (defn debug [ob & args]
;;   (cond
;;     (string? ob) (println "*DEBUG* " ob)

;;     (fn? ob)
;;     (do (println "*DEBUG* Call to '" ob "' with args '" args "'")
;;         (let [ret (apply ob args)]
;;           (println "*DEBUG* Returning '" ret "'")
;;           ret))))
