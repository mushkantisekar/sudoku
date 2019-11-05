(ns sudoku.engine-test
  (:require  #?(:clj [clojure.test :refer :all]
                :cljs [cljs.test :refer :all :include-macros true])))

(def sdk (->Sudoku (vec (range 1 82))))

(deftest set-cell!-t
  (let [sdk (set-cell sdk 1 1 42)
        val (get-cell sdk 1 1)]
    (is (= val 42))
    (is (thrown? Throwable (get-cell sdk 0 0)))
    (is (thrown? Throwable (set-cell sdk 0 0)))))

(deftest get-cell-t
  (is (= 1 (get-cell sdk 1 1)))
  (is (= 81 (get-cell sdk 9 9)))
  (is (thrown? Throwable (get-cell sdk 10 10))))

(deftest row-t
  (is (= #{1 2 3 4 5 6 7 8 9} (row sdk 1))))
