(ns sudoku.g2048
  (:require [clojure.set :as set]
            [ better-cond.core :as b]))

;; Utilities
(defmacro it-> [v & forms] `(as-> ~v ~'it ~@forms))
(defn exp [x n] (reduce * (repeat n x)))

;; Definitions
(def dirs #{:up :down :left :right})
(defrecord Board [tiles])
(declare add-random-tile)
(declare move-board)
(defn new-board [] (->Board (vec (repeat 16 nil))))

(defn random-tile-value [board] 2) ;; FIXME
;;(defn move [dir board] (if (= :left dir) board nil))
;; (defn powers [n] (->> (range n) (drop 1) (map #(exp 2 %))))

(defn add-random-tile [board]
  (let [tiles     (:tiles board)
        ind-tiles (map-indexed (fn [index t] [index t]) tiles)
        nil-tiles (filter #(nil? (second %)) ind-tiles)
        num-nil   (count nil-tiles)
        rnd-el    (if (= 0 num-nil) nil (rand-nth nil-tiles))
        rnd-ind   (if (nil? rnd-el) nil (first rnd-el))]
    (if (nil? rnd-el)
      board
      (assoc-in board [:tiles rnd-ind] (random-tile-value board)))))


(defn- tiles->vectors [dir tiles]
  (cond
    (= dir :left) (partition 4 tiles)
    (= dir :right) (map reverse (tiles->vectors :left tiles))
    (= dir :up) (for [i (range 4)] (take-nth 4 (drop i tiles)))
    (= dir :down) (map reverse (tiles->vectors :up tiles))
    :else nil))

(defn- vectors->tiles [dir vectors]
  (cond
    (= dir :left) (vec (reduce concat vectors))
    (= dir :right) (vectors->tiles :left (map reverse vectors))
    (= dir :up) (vec (reduce concat (apply mapv list vectors)))
    (= dir :down) (vectors->tiles :up (map reverse vectors))
    :else nil))

(defn- sum-row
  "Merges adjacent tiles with the same values. Assumes there are no
  nils interleaved with values (i.e. row was already compacted)"
  [row]
  (let [[a b & r] row]
    (cond
      (or (nil? a) (nil? b)) row
      (= a b) (it-> (+ a b)
                    (list it)
                    (concat it (sum-row (vec r)))
                    (concat it (list nil))
                    (vec it))
      :else (it-> (list a)
                  (concat it (sum-row (concat (list b) r)))))))

(defn- compact-row
  "Moves the tiles of the row to the left (towards the start)"
  [row]
  (let [row-len   (count row)
        compacted (->> row (filter #(not (nil? %))))
        pad-len   (- row-len (count compacted))
        pad       (repeat pad-len nil)]
    (vec (concat compacted pad))))

;; Moves elements in the row towards the beggining (the "left")
(defn- move-row [row] (-> row compact-row sum-row))
;; Moves the whole board to the left (by moving each row)
(defn- move-tiles [tiles] (map move-row tiles))

(defn move-board
  "Moves the tiles in the board in the given direction dir
  (:left :right :up or :down)"
  [dir board]
  (it-> (:tiles board)
        (tiles->vectors dir it)
        (move-tiles it)
        (vectors->tiles dir it)
        (->Board it)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Text based version of the game
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- print-board [board]
  (it-> (:tiles board)
        (map #(if (nil? %) "- " (str % " ")) it)
        (map-indexed (fn [ind el] (if (= 0 (rem (inc ind) 4)) (str el "\n") el)) it)
        (reduce str it)
        (str "\n" it)
        (println it)))

(declare new-game)
(defn finished? [board] (= 0 (it-> (:tiles board) (filter nil? it) (count it))))
(defn- finish-game [board]
  (println "Game finished: another one (Y/N)")
  (let [command (read-line)]
    (when (or (= "Y" (first command)) (= "y" (first command)))
      (new-game))))

(defn usage []
  (println "Use 'h'/'j'/'k'/'l' to move left, down, up or right, 'q' to finish"))

(defn new-game []
  (usage)
  (loop [b        (new-board)
         changed? true]
    (b/cond
      :let [board (if changed? (add-random-tile b) b)]
      :do (print-board board)
      (finished? board) (finish-game)

      :let [command (read-line)
            cmd     (first command)]
      (not (contains? #{\h\j\k\l\q} cmd)) (do (usage) (recur board false))

      (= \q cmd) (println "Game finished")

      :let [next-board
            (cond
              (= \h cmd) (move-board :left board)
              (= \l cmd) (move-board :right board)
              (= \j cmd) (move-board :down board)
              (= \k cmd) (move-board :up board))]
      (= next-board board) (recur board true)
      :else                (recur board false))))
