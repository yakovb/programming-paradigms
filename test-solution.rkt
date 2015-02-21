#lang racket

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

(define (mkset elem) ; this should make a cell struct, which is the set for the elem
  (if (zero? elem) 
      (set 1 2 3 4 5 6 7 8 9)
      (set elem)))

(define (relist in) ; this should make a row struct and return a list of them
  (if (empty? in)
      empty
      (cons (take in 9) (relist (drop in 9)))))

; take a "list of lists of ints" and return a "list of lists of sets"
(define (transform matrix)
    (foldr (lambda (row z) 
             (cons (map (lambda (cell) (mkset cell)) row) z))           
           empty
           matrix))

; take a "list of lists of sets" and return a" list of lists of ints"
(define (setify x) 
    (foldr (lambda (row z)
             (cons (map (lambda (cell) (set->list cell)) row) z))
           empty
           x))

; take a "list of lists of A" and return "a list of lists of B"
(define (process-list func input)
  (foldr (lambda (row z)
           (cons (map (lambda (a) (func a)) row) z))
         empty
         input))

; create cell structs with one pass through the puzzle
(define (super-transform input)
  (let* ([row 0]
         [col 0])
    (foldr (lambda (row-input z)
             (cons (map (lambda (elem) (cell (mkset elem) (+ 1 row) (+ 1 col))) row-input) z))
           empty
           input)))
             
          
; test for defining a cell struct
(struct cell (data row col))