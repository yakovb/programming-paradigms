#lang racket
(require "sudoku.rkt"
         rackunit
         rackunit/text-ui)

(define sudoku-tests
  (test-suite
   "Wrapper suite for all tests"
   (test-suite
    "cell struct tests"
    (let ([c1 (cell (set 1 2 3) 1 1 'upper-left #t)]
          [c2 (make-cell (set 3 4 5) 2 2 'lower-right)])
      
      (test-case
       "basic cell struct contsruction"
       (check-equal? (cell-data c1) (set 1 2 3) "error in cell data getter")
       (check-equal? (cell-row c1) 1 "error in cell row getter")
       (check-equal? (cell-col c1) 1 "error in cell col getter")
       (check-equal? (cell-box c1) 'upper-left "error in cell box getter")
       (check-equal? (cell-singleton-checked? c1) #t "error in cell singleton-checked getter"))
      
      (test-case
       "cell struct construction with default parameter"
       (check-equal? (cell-data c2) (set 3 4 5) "error in cell data getter")
       (check-equal? (cell-row c2) 2 "error in cell row getter")
       (check-equal? (cell-col c2) 2 "error in cell col getter")
       (check-equal? (cell-box c2) 'lower-right "error in cell box getter")
       (check-equal? (cell-singleton-checked? c2) #f "error in cell singleton-checked getter"))))
      
   
   (test-suite 
    "Puzzle input-processing helper functions"
    (let ([input '(0 1)]
          [f make-set-of-possible-values])
      
      (test-case
       "set construction"
       (check-equal? (f (first input)) (set 1 2 3 4 5 6 7 8 9) "error in making set with 0 input")
       (check-equal? (f (second input)) (set 1) "error in making set with non-0 input"))))))
  
(run-tests sudoku-tests)