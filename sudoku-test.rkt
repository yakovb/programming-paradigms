#lang racket
(require rackunit
         rackunit/text-ui
         "sudoku.rkt")

(define sudoku-tests
  (test-suite
   "Tests for cell struct"
   (let ([c1 (cell (set 1 2 3) 1 1 'upper-left #t)]
         [c2 (make-cell (set 3 4 5) 2 2 'lower-right)])
     
     (test-case
      "basic cell struct contsruction"
      (check-equal? (set 1 2 3) (cell-data c1)))
     
     (test-case
      "cell struct construction with default parameter"
      (fail "default fail fancy struct")))))
  
(run-tests sudoku-tests)