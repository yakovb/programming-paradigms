#lang racket
(require rackunit
         rackunit/text-ui
         "sudoku.rkt")

(define sudoku-tests
  (test-suite
   "Tests for cell struct"
   
   (test-case
    "basic cell struct contsruction"
    (fail "default fail easy struct"))
   
   (test-case
    "cell struct construction with default parameter"
    (fail "default fail fancy struct"))))

(run-tests sudoku-tests)