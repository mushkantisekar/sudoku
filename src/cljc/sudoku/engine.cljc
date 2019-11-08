(ns sudoku.engine
  (:require [clojure.set :as st]))


(defn debug [ob & args]
  (cond
    (string? ob) (println "*DEBUG* " ob)

    (fn? ob)
    (do (println "*DEBUG* Call to '" ob "' with args '" args "'")
        (let [ret (apply ob args)]
          (println "*DEBUG* Returning '" ret "'")
          ret))))

(def possible-vals #{1 2 3 4 5 6 7 8 9})

(defn possible-val? [n] (contains? possible-vals n))
(defn map-sudoku [f sk] (vec (map #(vec (map f %)) sk)))
(defn reduce-sudoku [f val sk] (reduce f val (reduce concat sk)))
(defn map-indexed-sudoku [f sk]
  (vec (map-indexed
        (fn [r rv]
          (vec (map-indexed
                (fn [c v] (f r c v))
                rv)))
        sk)))

(defn transpose [sk] (vec (for [i (range 9)] (vec (map #(get % i) sk)))))
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
     (fn [r c v]
       (let [cell-opts (get-cell sk-opts r c)]
         (if (= 1 (count cell-opts))
           (first cell-opts) ;; FIXME: check value is compatible with options
           v)))
     sk)))

(defn complete-sudoku [sk]
  (if (invalid? sk)
    nil
    (loop [orig sk]
      (let [updt (complete-sudoku-once orig)]
        (if (= orig updt)
          orig
          (recur updt))))))

(declare solve-sudoku)

(defn try-opts-at [sk r c opts]
  (->> (lazy-seq opts)
       (map #(set-cell sk r c %))
       (map solve-sudoku)
       (reduce concat) ; every solve sudoku returns a list
       (filter #(not (or (empty? %) (nil? %))))))

(defn solve-open-sudoku [sk]
  (let [opts (options sk)]
    (->> opts
         (map-indexed-sudoku (fn [r c v] [r c (count v)])) ; Take only cells with
         (reduce concat)                                   ; multiple options
         (filter #(> (% 2) 1))                             ; available
         (first)
         ;(map (fn [[r c _]] (try-opts-at sk r c (get-cell opts r c))))
         ((fn [[r c _]] (try-opts-at sk r c (get-cell opts r c))))
         (filter #(not (or (empty? %) (nil? %)))))))

(defn solve-sudoku [sk]
  (let [sk (complete-sudoku sk)]
    (if (solved? sk)
        (list sk)
        (solve-open-sudoku sk))))

;; (defn mod-swap-rows [sk block r1 r2]
;;   (let [r1 (+ r1 (* 3 block))
;;         r2 (+ r2 (* 3 block))]
;;     (assoc sk r1 (sk r2) r2 (sk r1))))

;; (defn mod-swap-cols [sk block c1 c2]
;;   (-> sk transpose (mod-swap-rows block c1 c2) transpose))

;; (defn mod-swap-blocks-hor [sk b1 b2]

;;   )

;; (defn mod-swap-blocks-ver [sk b1 b2]
;;   (when (or (< 0 b1) (> 2 b1)
;;             (< 0 b2) (> 2 b2))
;;     (throw (-> RuntimeException "0 <= b[1|2] <= 2")))
;;   sk) ;; FIXME

(let [_ nil]
  (def empty-sudoku (vec (for [i (range 9)] (vec (for [j (range 9)] nil)))))

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

  (def invalid-sudoku
    [[2 2 3 4 5 6 7 8 9]
     [4 5 6 7 8 9 1 2 3]
     [7 8 9 1 2 3 4 5 6]
     [2 3 4 5 6 7 8 9 1]
     [5 6 7 8 9 1 2 3 4]
     [8 9 1 2 3 4 5 6 7]
     [3 4 5 6 7 8 9 1 2]
     [6 7 8 9 1 2 3 4 5]
     [9 1 2 3 4 5 6 7 8]])

  (def directly-solvable-sudoku
    [[1 _ 3 4 _ 6 7 _ _]
     [4 _ 6 _ 8 _ _ _ 3]
     [_ 8 9 _ 2 3 _ 5 _]
     [2 _ 4 _ _ 7 _ _ _]
     [_ _ 7 _ 9 _ _ 3 _]
     [8 _ _ 2 _ 4 _ 6 _]
     [_ _ 5 _ 7 _ _ _ 2]
     [6 _ 8 _ _ 2 _ 4 _]
     [9 _ 2 _ 4 _ 6 _ 8]])

  (def open-sudoku
    [[1 _ _ 4 _ 6 7 _ _]
     [4 _ 6 _ 8 _ _ _ 3]
     [_ 8 _ _ 2 3 _ 5 _]
     [2 _ 4 _ _ 7 _ _ _]
     [_ _ 7 _ 9 _ _ 3 _]
     [8 _ _ 2 _ 4 _ 6 _]
     [_ _ 5 _ 7 _ _ _ 2]
     [6 _ 8 _ _ 2 _ 4 _]
     [9 _ _ _ 4 _ 6 _ 8]]))
