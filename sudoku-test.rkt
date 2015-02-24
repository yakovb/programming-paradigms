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
          [c2 (make-cell (set 3 4 5) 2 2 'lower-right)]
          [c3 (make-cell (set 3 4 5) 2 2 'lower-right #t)])
      
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
       (check-equal? (cell-singleton-checked? c2) #f "error in cell singleton-checked getter"))
      
      (test-case
       "cell struct with explicit singleton-checked? parameter"
       (check-equal? (cell-singleton-checked? c3) #t "singleton-checked should be #t as per constructor"))))
      
   
   (test-suite 
    "Puzzle input-processing helper functions"
    (let ([input '(0 1)]
          [f make-set-of-possible-values])
      
      (test-case
       "set construction"
       (check-equal? (f (first input)) (set 1 2 3 4 5 6 7 8 9) "error in making set with 0 input")
       (check-equal? (f (second input)) (set 1) "error in making set with non-0 input"))))
   
   
   (test-suite
    "Box reference symbol maker"
    (let ([f box-lookup])
      
      (test-case
       "upper boxes"
       (check-eq? (f 1 3) 'upper-left "error with upper-left symbol")
       (check-eq? (f 2 5) 'upper-middle "error with upper-middle symbol")
       (check-eq? (f 3 9) 'upper-right "error with upper-right symbol"))
      
      (test-case
       "middle boxes"
       (check-eq? (f 4 3) 'middle-left "error with middle-left symbol")
       (check-eq? (f 5 5) 'middle-middle "error with middle-middle symbol")
       (check-eq? (f 6 9) 'middle-right "error with middle-right symbol"))
      
      (test-case
       "lower boxes"
       (check-eq? (f 7 3) 'lower-left "error with lower-left symbol")
       (check-eq? (f 8 5) 'lower-middle "error with lower-middle symbol")
       (check-eq? (f 9 9) 'lower-right "error with lower-right symbol"))
      
      (test-case
       "box out of bounds errors"
       (check-exn exn:fail? (lambda () (f 3 10)) "column error should have been thrown") 
       (check-exn exn:fail? (lambda () (f 10 6)) "row error should have been thrown"))))
   
   (test-suite
    "Processing of nested elements"
    (let ([f process-nested-elements]
          [ll-numbers '((1 2) (3 4))]
          [ll-sets (list (list (set 1 2) (set 3 4)) (list (set 5 6) (set 7 8)))]
          [ll-strings '(("this" "that") ("who" "whatever"))])
      
      (test-case 
       "convert numbers to sets"
       (let ([result (f (lambda (e) (set e)) ll-numbers)])
         (check-equal? (caar result) (set 1) "problem in numbers to sets")
         (check-equal? (cdr result) (list (list (set 3) (set 4))) "problem in numbers to sets")))
      
;      (test-case
;       "convert sets to lists"
;       (let ([result (f (lambda (e) (reverse (set->list e))) ll-sets)])
;         (check-equal? (caar result) '(1 2) "problem in sets to list")
;         (check-equal? (cdr result) '(((5 6) (7 8))) "problem in sets to list")))
      
      (test-case
       "convert srings to numbers"
       (let ([result (f (lambda (e) (string-length e)) ll-strings)])
         (check-equal? (caar result) 4 "problem in srings to numbers")
         (check-equal? (caadr result) 3 "problem in srings to numbers")))))
   
   (test-suite
    "Transform sudoku puzzle from list of list of number to list of list of sets"
    (let ([result (transform '((1 0) (3 4)))])
      (check-equal? (car result) (list (set 1) (set 1 2 3 4 5 6 7 8 9)) "problem making ll-set")
      (check-equal? (caadr result) (set 3) "problem making ll-set")))
   
   (test-suite
    "Create flattened list of cell structs out of transformed puzzle"
    (let* ([input (list (list (set 1) (set 2) (set 3) (set 4) (set 5) (set 6) (set 7) (set 8) (set 9))
                        (list (set 1 2 3 4 5 6 7 8 9) (set 2) (set 3) (set 4) (set 5) (set 6) (set 7) (set 8) (set 9)))]
           [result (cells-list input)])
      (test-case
       "First element of the result (which is in reverse row order)"
       (let ([elem (first result)])
         (check-equal? (cell-data elem) (set 1 2 3 4 5 6 7 8 9) "expected set of all possibilities")
         (check-equal? (cell-row elem) 2 "row should be 2")
         (check-equal? (cell-col elem) 1 "col should be 1")
         (check-eq? (cell-box elem) 'upper-left "box should be upper-left")
         (check-false (cell-singleton-checked? elem) "singleton-checked? should be #f")))
      
      (test-case
       "Last element of the result (which is in reverse row order)"
       (let ([elem (last result)])
         (check-equal? (cell-data elem) (set 9) "expected set with single val 9")
         (check-equal? (cell-row elem) 1 "row should be 1")
         (check-equal? (cell-col elem) 9 "col should be 9")
         (check-eq? (cell-box elem) 'upper-right "box should be upper-right")
         (check-false (cell-singleton-checked? elem) "singleton-checked? should be #f")))))
   
   (test-suite
    "Behaviour of separate-singletons function"
    (let ([f separate-singletons]
          [dbl (lambda (x) (* 2 x))]
          [in '(1 2 3 4 5 6)])
      
      (test-case
       "Situation where all elements fail the predicate"
       (let ([pred string?])
         (check-equal? (f pred #:func-fails dbl in) (list 2 4 6 8 10 12))))
         
      (test-case
       "Situation where all elements pass the predicate"
       (let ([pred number?])
         (check-equal? (f pred #:func-fails dbl in) (list 1 2 3 4 5 6))))
       
      (test-case
       "Situation where some elements pass the predicate"
       (let ([pred even?])
         (check-equal? (f pred #:func-fails dbl in) (list 2 4 6 2 6 10))))))
   
   ;;TODO test-suite separate-singletons with action on passes
   
   ;;TODO test-suite separate-singletons with no action ie default funcs for passes and fails
   
   (test-suite
    "Determine if cell is a singleton that should be processed"
    (let ([sgl-NO-chkd-NO (cell (set 1 2) 1 1 'upper-left #f)]
          [sgl-YES-chkd-NO (cell (set 2) 1 1 'upper-left #f)]
          [sgl-YES-chkd-YES (cell (set 2) 1 1 'upper-left #t)])
      (check-eq? (valid-singleton? sgl-NO-chkd-NO) #f) 
      (check-eq? (valid-singleton? sgl-YES-chkd-NO) #t)
      (check-eq? (valid-singleton? sgl-YES-chkd-YES) #f)))
   
   (test-suite
    "Getting cell associations"
    
    (test-case
     "middle-middle cell"
     (let* ([c (make-cell (set 1 2) 4 5 'middle-middle)]
           [c-assoc (cell-associations c)])
       (check-eq? (first c-assoc) 4 "row should be 4")
       (check-eq? (second c-assoc) 5 "column should be 5")
       (check-eq? (last c-assoc) 'middle-middle' "box should be middle-middle"))))
   
   (test-suite
    "Make cell without certain numbers"
    (let ([c-all (make-cell (set 1 2 3 4 5 6 7 8 9) 1 1 'upper-left)]      
          [c-three (make-cell (set 3 6 9) 2 2 'upper-left)]          
          [c-sngl (make-cell (set 5) 3 3 'upper-left)])
      
      (test-case
       "removing 5 from cell with full set"
       (check-equal? (cell-data (make-cell-without c-all 5)) (set 1 2 3 4 6 7 8 9)))
      (test-case 
       "removing 9 from a cell with three-set"
       (check-equal? (cell-data (make-cell-without c-three 9)) (set 3 6)))
      (test-case 
       "removing val from singleton cell causes error"
       (check-exn exn:fail? (make-cell-without c-sngl 5)))))
      
      ))
  
(run-tests sudoku-tests)