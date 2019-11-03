(ns sudoku.doo-runner
  (:require [doo.runner :refer-macros [doo-tests]]
            [sudoku.core-test]))

(doo-tests 'sudoku.core-test)
