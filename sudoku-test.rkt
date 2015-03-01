#lang racket
(require "sudoku.rkt"
         rackunit
         rackunit/text-ui)

(define puzzle '((0 2 5 0 0 1 0 0 0)
                (1 0 4 2 5 0 0 0 0)
                (0 0 6 0 0 4 2 1 0)
                (0 5 0 0 0 0 3 2 0)
                (6 0 0 0 2 0 0 0 9)
                (0 8 7 0 0 0 0 6 0)
                (0 9 1 5 0 0 6 0 0)
                (0 0 0 0 7 8 1 0 3)
                (0 0 0 6 0 0 5 9 0)
                ))

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
    "Transform back a cells list into a matrix"
    (let ([solved (list (cell (set 9) 3 3 'ul #t)
                        (cell (set 3) 1 3 'ul #t)
                        (cell (set 5) 2 2 'ul #t)
                        (cell (set 1) 1 1 'ul #t)
                        (cell (set 8) 3 2 'ul #t)
                        (cell (set 6) 2 3 'ul #t)
                        (cell (set 7) 3 1 'ul #t)
                        (cell (set 2) 1 2 'ul #t)
                        (cell (set 4) 2 1 'ul #t))]
          [unsolved (list (cell (set 7 8 9) 3 3 'ul #f)
                          (cell (set 3) 1 3 'ul #t)
                          (cell (set 4 5 6) 2 2 'ul #f)
                          (cell (set 1 2 3) 1 1 'ul #f)
                          (cell (set 8) 3 2 'ul #t)
                          (cell (set 6) 2 3 'ul #t)
                          (cell (set 7) 3 1 'ul #t)
                          (cell (set 2) 1 2 'ul #t)
                          (cell (set 4) 2 1 'ul #t))])
      (test-case
       "Solved puzzle"
       (let ([actual (transform-back solved 3)]
             [expected (list (list 1 2 3)
                             (list 4 5 6)
                             (list 7 8 9))])
         (check-equal? (first actual) (first expected) "First row should be 1 2 3")
         (check-equal? (second actual) (second expected) "Second row should be 4 5 6")
         (check-equal? (third actual) (third expected) "Third row should be 7 8 9")))
      
      (test-case
       "Unsolved puzzle"
       (let ([actual (transform-back unsolved 3)]
             [expected (list (list (set 1 2 3) 2 3)
                             (list 4 (set 4 5 6) 6)
                             (list 7 8 (set 7 8 9)))])
         (check-equal? (first actual) (first expected) "First row should be [1 2 3] 2 3")
         (check-equal? (second actual) (second expected) "Second row should be 4 [4 5 6] 6")
         (check-equal? (third actual) (third expected) "Third row should be 7 8 [7 8 9]")))))
   
   
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
       (check-equal? (cell-data (make-cell-without (set 5) c-all)) (set 1 2 3 4 6 7 8 9)))
     
      (test-case 
       "removing 9 from a cell with three-set"
       (check-equal? (cell-data (make-cell-without (set 9) c-three)) (set 3 6)))
     
      (test-case 
       "removing val from singleton cell causes error"
       (check-exn exn:fail? (lambda () (make-cell-without (set 5) c-sngl)) 
                  "FAIL: attempted to make a cell with an empty set as data. Something has gone wrong!"))))
   
   
   (test-suite
    "Make a singleton cell"
    (let ([c1 (make-cell (set 1 2 3 4) 1 1 'upper-left)]
          [c2 (make-cell (set 1) 1 1 'upper-left)])
      
      (test-case
       "set size > 1"
       (let ([c1-single (make-singleton c1 3)])
         (check-equal? (cell-data c1-single) (set 3) "new set data should only contain 3")
         (check-eq? (cell-row c1-single) 1 "row should be the same")
         (check-eq? (cell-col c1-single) 1 "col should be the same")
         (check-eq? (cell-box c1-single) 'upper-left "box should be the same")))
      
      (test-case
       "set size = 1"
       (let ([c2-single (make-singleton c2 1)])
         (check-equal? (cell-data c2-single) (set 1) "new set data should only contain 1")
         (check-eq? (cell-row c2-single) 1 "row should be the same")
         (check-eq? (cell-col c2-single) 1 "col should be the same")
         (check-eq? (cell-box c2-single) 'upper-left "box should be the same")))
      
      (test-case
       "set size > 1 but cell doesn't contain singleton value"
       (check-exn exn:fail? (lambda () (make-singleton c1 5)) 
                  "supplied singleton value is not possible in this cell"))))
   
   
   (test-suite
    "Toggle checked singletons"
    (test-case
     "should accept all input as it's caller's responsibility to ensure correct args passed in"
     (let* ([clist (toggle-checked-singletons 
                    (list (make-cell (set 1) 1 1 'upper-left) (make-cell (set 1 2) 1 1 'upper-left #t)))]
            [c1 (first clist)]
            [c2 (last clist)])
       (check-eq? (cell-singleton-checked? c1) #t)
       (check-eq? (cell-singleton-checked? c2) #t))))
   
   
   (test-suite
    "Remove vals from associated cells"
    (let* ([remove (list (make-cell (set 1) 1 1 'ul)
                        (make-cell (set 2) 3 3 'ul))]
          [target (list (make-cell (set 1 2 3 4) 2 2 'ul)
                        (make-cell (set 5 6 7) 1 2 'ul)
                        (make-cell (set 2 3) 1 3 'ul)
                        (make-cell (set 1) 9 9 'lr))]
          [result (remove-from-associated remove target)])
      
      (test-case
       "Result should only contain target cells"
       (check-eq? (length result) 4 "There should only be 4 cells in result"))
      
      (test-case 
       "Correctly dealt with associated targets"
       (check-equal? (cell-data (first result)) (set 3 4))
       (check-equal? (cell-data (second result)) (set 5 6 7))
       (check-equal? (cell-data (third result)) (set 3)))
      
      (test-case 
       "correctly dealth with unassociated target"
       (check-equal? (cell-data (fourth result)) (set 1)))))
   
   
   (test-suite
    "Associated cells"
    
    (test-case
     "cells with an associations"
     (let ([c1 (make-cell (set 2) 1 1 'upper-left)]
           [c2 (make-cell (set 4 5) 3 3 'upper-left)])
       (check-not-false (associated-cells? c1 c2) "cells should be associated")))
    
    (test-case
     "cells without an associations"
     (let ([c1 (make-cell (set 2) 1 1 'upper-left)]
           [c2 (make-cell (set 4 5) 9 9 'lower-right)])
       (check-false (associated-cells? c1 c2) "cells should not be associated"))))
   
   
   (test-suite
    "Reduce singletons"
    (let*-values ([(c-sngl) (make-cell (set 1) 1 1 'upper-left)]
                   [(c-sngl2) (make-cell (set 2) 2 8 'upper-right)]
                   [(c1-touch) (make-cell (set 1 2 3) 2 2 'upper-left)]
                   [(c2-leave) (make-cell (set 1 5 7) 9 9 'lower-right)]
                   [(c3-touch) (make-cell (set 1 9) 1 9 'upper-right)]
                   [(lst1 flag1) (reduce-singletons (list c1-touch c2-leave c-sngl c3-touch))]
                   [(lst2 flag2) (reduce-singletons (list c1-touch c-sngl c2-leave c-sngl2 c3-touch))])
                  
      (test-case
       "list with no singletons"
       (let ([lst (list (make-cell (set 1 2 3) 1 1 'upper-left)
                        (make-cell (set 4 6 7) 6 6 'middle-middle)
                        (make-cell (set 6 7) 9 9 'lower-right))])
         (let-values ([(lstf f) (reduce-singletons lst)])
           (check-false f "should be false as no singletons in list"))))
                 
      (test-case
       "list with one singleton"
       (check-true flag1 "Should be true as list amended")
       (check-true (cell-singleton-checked? (first lst1)) "singleton should be processed")
       (check-equal? (cell-data (second lst1)) (set 2 3) "c1-touch should have had its set reduced")
       (check-equal? (cell-data (third lst1)) (set 1 5 7) "c2-leave should not have had its set reduced")
       (check-equal? (cell-data (fourth lst1)) (set 9) "c3-touch should have had its set reduced"))
    
      (test-case
       "list with two singletons"
       (check-true flag2 "Should be true as list amended")
       (check-true (cell-singleton-checked? (first lst2)) "first singleton should be processed")
       (check-true (cell-singleton-checked? (second lst2)) "second singleton should be processed")
       (check-equal? (cell-data (third lst2)) (set 3) "c1-touch should have had its set reduced to 2")
       (check-equal? (cell-data (fourth lst2)) (set 1 5 7) "c2-leave should not have had its set reduced")
       (check-equal? (cell-data (fifth lst2)) (set 9) "c3-touch should have had its set reduced to 9"))))
     
   
   
   (test-suite 
    "Finding single numbers in cell list"
    (let ([lst (list (make-cell (set 1 3) 1 1 'ul)
                      (make-cell (set 3 6 7) 2 2 'ul)
                      (make-cell (set 1 6 8) 3 3 'ul))]
           [test-pass (make-cell (set 7 8 9) 1 3 'ul)]
           [test-fail (make-cell (set 7 8 1) 1 3 'ul)])
      
      (test-case 
       "With a singleton to find"
       (let-values ([(result flag) (make-single-if-poss test-pass lst)])
         (check-equal? (cell-data result) (set 9) "9 should have been found")
         (check-true flag "flag should signify change in cell")))
       
      (test-case
       "With nothing to find"
       (let-values ([(result flag) (make-single-if-poss test-fail lst)])
         (check-equal? (cell-data result) (set 7 8 1) "Nothing should have been found")
         (check-false flag "flag should signify no change in cell")))))
   
  
   (test-suite
    "Find singles in a list of cells"
    (let* ([no-assoc (list (make-cell (set 1 3) 9 9 'lr)
                           (make-cell (set 1 3) 9 8 'lr))]
           [one-single (list (make-cell (set 1 3 6) 1 1 'ul)
                            (make-cell (set 1 3 6 9) 2 2 'ul)
                            (make-cell (set 1 6 3) 3 3 'ul))]
          [one-real-one-fake-single (list (make-cell (set 1 3 6 8) 1 1 'ul)
                             (make-cell (set 1 3 6 9) 2 2 'ul)
                             (make-cell (set 1 6 3 8) 3 3 'ul))]
          [checked (list (make-cell (set 4) 1 2 'ul #t)
                         (make-cell (set 5) 1 3 'ul #t))]
          [one+noassoc (append one-single no-assoc)]
          [two+noassoc (append one-real-one-fake-single no-assoc)]
          [two+checked+noassoc (append checked one-real-one-fake-single)])
      
      (test-case
       "Finding single val in mixed cells with a valid singleton"
       (let-values ([(result flag) (find-single-val-in-set two+checked+noassoc)])
         (check-true flag "changes should have been made to the input")
         (check-true flag "changes should have been made to the input")
         (check-true (valid-singleton? (first result)) "first cell should be singleton")
         (check-equal? (cell-data (first result)) (set 4) "data of first cell should be 4")
         (check-true (valid-singleton? (second result)) "second cell should be singleton")
         (check-equal? (cell-data (second result)) (set 5) "data of first cell should be 5")))
      
      (test-case
        "Finding single val in mixed cells without a valid singleton"
        (let-values ([(result flag) (find-single-val-in-set (append no-assoc checked))])
          (check-false flag "no changes should have been made to the input")))
      
      (test-case
       "Candidate cells with one singleton to process"
       (let-values ([(result flag) (singles-in-candidate-cells one+noassoc)])
         (check-true flag "changes should have been made to the input")
         (check-equal? (cell-data (fifth result)) (set 9) "Should have made singleton with 9")
         (check-eq? (cell-row (fifth result)) 2 "row should be 2")
         (check-eq? (cell-col (fifth result)) 2 "col should be 2")
         (check-eq? (cell-box (fifth result)) 'ul "box should be ul")))
      
      (test-case
       "Candidate cells with one real and one fake singleton to process"
       (let-values ([(result flag) (singles-in-candidate-cells two+noassoc)])
         (check-true flag "changes should have been made to the input")
         (check-equal? (cell-data (fifth result)) (set 9) "Should have made singleton with 9")
         (check-eq? (cell-row (fifth result)) 2 "row should be 2")
         (check-eq? (cell-col (fifth result)) 2 "col should be 2")
         (check-eq? (cell-box (fifth result)) 'ul "box should be ul")
         
         (check-equal? (cell-data (first result)) (set 1 6 3 8) "Nothing should be changed in the set")
         (check-eq? (cell-row (first result)) 3 "row should be 3")
         (check-eq? (cell-col (first result)) 3 "col should be 3")
         (check-eq? (cell-box (first result)) 'ul "box should be ul")))
      
      (test-case
       "Associated cells with one singleton to process"
       (let-values ([(result flag) (singles-in-associated-cells 
                                    one-single)])
         (check-true flag "flag should signify change was made")
         (check-equal? (cell-data (second result)) (set 9) "Should have made singleton with 9")
         (check-eq? (cell-row (second result)) 2 "row should be 2")
         (check-eq? (cell-col (second result)) 2 "col should be 2")
         (check-eq? (cell-box (second result)) 'ul "box should be ul")))
      
      (test-case
       "Associated cells with one real and one fake singleton to process"
       (let-values ([(result flag) (singles-in-associated-cells 
                                    one-real-one-fake-single)])
         (check-true flag "flag should signify change was made")
         (check-equal? (cell-data (second result)) (set 9) "Should have made singleton with 9")
         (check-eq? (cell-row (second result)) 2 "row should be 2")
         (check-eq? (cell-col (second result)) 2 "col should be 2")
         (check-eq? (cell-box (second result)) 'ul "box should be ul")
         (check-equal? (cell-data (third result)) (set 1 6 3 8) "Nothing should be changed in the set")
         (check-eq? (cell-row (third result)) 3 "row should be 3")
         (check-eq? (cell-col (third result)) 3 "col should be 3")
         (check-eq? (cell-box (third result)) 'ul "box should be ul")))))
   
   ))
  
(run-tests sudoku-tests)