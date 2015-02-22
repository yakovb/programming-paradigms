#lang racket
(require "sudoku.rkt"
         rackunit
         rackunit/text-ui)

(define sudoku-tests
  (test-suite
   "Tests for cell struct"
   (let ([c1 (cell (set 1 2 3) 1 1 'upper-left #t)]
         [c2 (make-cell (set 3 4 5) 2 2 'lower-right)])
     
     (test-case
      "basic cell struct contsruction"
      (check-equal? (set 1 2 3) (cell-data c1) "error in cell data getter")
      (check-equal? 2 (cell-row c1) "error in cell row getter"))
     
     (test-case
      "cell struct construction with default parameter"
      (fail "default fail fancy struct")))))
  
(run-tests sudoku-tests)