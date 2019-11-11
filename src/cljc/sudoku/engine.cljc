(ns sudoku.engine
  (:require [clojure.set :as set]))

(def possible-vals #{1 2 3 4 5 6 7 8 9})

(defn- possible-val? [n] (contains? possible-vals n))
(defn- reduce-sudoku [f val sk] (reduce f val (reduce concat sk)))
(defn- map-indexed-sudoku [f sk]
  (vec (map-indexed (fn [r rv] (vec (map-indexed (fn [c v] (f r c v)) rv))) sk)))

(defn- get-value [sk r c] (-> sk (get r) (get c)))
(defn- set-value [sk r c v] (assoc-in sk [r c] v))
(defn- row [sk n] (get sk n))
(defn- col [sk n] (vec (for [i (range 9)] (get-value sk i n))))

(defn- sqr [sk r c] ;; Returns the 3x3 square containing row r and column c
  (let [ir (* 3 (quot r 3))
        ic (* 3 (quot c 3))]
    (vec (for [i (range 3) j (range 3)] (get-value sk (+ ir i) (+ ic j))))))

(defn options-cell [sk r c v]
  (let* [sk (set-value sk r c nil)
         vals (into #{} (set/union (row sk r)
                                   (col sk c)
                                   (sqr sk r c)))
         opts (set/difference possible-vals vals)]
    (if-not (possible-val? v) opts (if (contains? opts v) #{v} #{}))))

(defn options [sk] (map-indexed-sudoku (fn [r c v] (options-cell sk r c v)) sk))
(defn solved? [sk] (reduce-sudoku #(and %1 (= 1 (count %2))) true (options sk)))

(defn invalid? [sk] ; Is the current sudoku invalid?
  (->> (options sk)
       (map-indexed-sudoku
        (fn [r c v] (or (contains? v (get-value sk r c))
                        (not (possible-val? (get-value sk r c))))))
       (flatten)
       (reduce #(and %1 %2) true)
       not))

(defn complete-sudoku-once [sk] ; Tries to fill in all cells whose content
  (let [sk-opts (options sk)]   ; can be deducted
    (map-indexed-sudoku
     (fn [r c v] (let [cell-opts (get-value sk-opts r c)]
                   (if (= 1 (count cell-opts)) (first cell-opts) v)))
     sk)))

(defn complete-sudoku [sk] ; Fills in all cells whose content can be deducted
  (if (invalid? sk)
    nil
    (loop [orig sk]
      (let [updt (complete-sudoku-once orig)]
        (if (= orig updt) orig (recur updt))))))

(declare -solve-sudoku)

(defn- solution-seq [sol-tree] ; Converts the solution tree in a lazy sequence of
  (filter vector? (tree-seq #(not (vector? %)) identity sol-tree)))  ; solutions

(defn- try-opts-at [sk r c opts random?] ; Tries all options at row r and col c
  (->> opts
       (map #(set-value sk r c %))
       (map #(-solve-sudoku % random?))))

(defn select-row-less-options [rows]
  ;; #_(println "Call to select-row-less-options '" rows "'")
  (if (empty? rows)
    (first rows)
    (reduce (fn [acc n]
              (if (<= (get acc 2) (get n 2)) 
                acc
                n))
            rows)))

(defn- solve-open-sudoku [sk random?] ; Solves a sudoku without any determined
  (let [opts      (options sk)       ; Individual cell
        selec-fn (if random?
                   (fn [coll] (if (empty? coll) nil (rand-nth coll)))
                   select-row-less-options)]
    (->> opts
         (map-indexed-sudoku (fn [r c v] [r c (count v)]))
         (reduce concat)
         (filter #(> (% 2) 1)) ; Take only cells with multiple options available
         selec-fn
         ((fn [[r c _]] (try-opts-at sk r c (get-value opts r c) random?))))))

(defn- -solve-sudoku [sk random?]
  (let [sk (complete-sudoku sk)]
    (if (solved? sk)
      (list sk)
      (solve-open-sudoku sk random?))))

(defn solve-sudoku
  "Finds all the solutions to a given solutions, returning them as a lazy seq.
  If random is true, then the order of the solutions would have a random
  component, although close solutions on the sequence will be very similar
   between themselves"
  ([sk]         (solve-sudoku sk false))
  ([sk random?] (solution-seq (-solve-sudoku sk random?))))

(defn solvable?
  "Returns true if the given sudoku has at least one solution, false otherwise"
  [sk]
  (-> sk (solve-sudoku) (first) nil? not))

(defn uniq-solution?
  "Returns true if the given sudoku has at one and only onesolution,
  false otherwise"
  [sk]
  (let [sols (solve-sudoku sk)
        sol1 (first sols)
        sol2 (second sols)]
    (and (not (nil? sol1))
         (nil? sol2))))

(defn remove-random-element
  "Removes a random element from the sudoku while ensuring that the resulting
  sudoku has only one solution. If no such element exists (all sudokus with
  one element less have not unique solutions) then nil is returned
  CAUTION: this function can currently be very slow, when the input sudoku
  has a reduced number of values (less than 20-23)"
  [sk]
  (->> sk
       (map-indexed-sudoku (fn [r c v] [r c v (rand)]))
       (reduce concat)
       (filter #(possible-val? (nth % 2)))
       (sort-by #(nth % 3))
       (map #(set-value sk (nth % 0) (nth % 1) nil))
       (filter uniq-solution?)
       (first)))

(defn remove-elements
  "Keeps removing elements by calling remove-random-element until no more
  elements can be removed (and keep a unique solution) of max elements have
  been removed.
  CAUTION: this function is always very slow"
  [sk max]
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
     [_ _ _ _ _ _ _ _ _]]))

(defn generate-random-sudoku
  "Generates a random sudoku by calculating sequence with all solutions to
  an empty sudoku and picking the first one"
  []
  (first (solve-sudoku sk-empty true)))


;; (defn debug [ob & args]
;;   (cond
;;     (string? ob) (println "*DEBUG* " ob)

;;     (fn? ob)
;;     (do (println "*DEBUG* Call to '" ob "' with args '" args "'")
;;         (let [ret (apply ob args)]
;;           (println "*DEBUG* Returning '" ret "'")
;;           ret))))
