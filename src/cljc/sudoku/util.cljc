(ns sudoku.util)

(defn foo-cljc [x]
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(def sudoku (atom (repeat 81 nil)))
(defn coords->pos [r c] (-> r (- 1) (* 9) (+ c) (- 1)))
(defn -row [sd n] (map #(sd (coords->pos n %)) (range 9)))
(defn row [n] (-row @sudoku n))
(defn -column[sd n] (map #(sd (coords->pos % n)) (range 9)))
(defn column [n] (-column @sudoku n))
(defn set-cell! [r c val]
  (swap! sudoku
         #(atom (assoc %
                       (coords->pos r c)
                       val))))

(defn get-cell [r c] (get @sudoku (coords->pos r c)))
