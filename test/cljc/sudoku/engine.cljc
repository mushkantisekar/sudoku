(ns sudoku.engine
  (:require [sudoku.engine :as sut]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])))

(t/deftest test-options-cell
  (t/is (= #{2 5} (sut/options-cell sk-directly-solvable 0 1 nil)))
  (t/is (= #{5} (sut/options-cell sk-directly-solvable 0 4 nil)))
  (t/is (= #{3} (sut/options-cell sk-directly-solvable 0 2 3))))
