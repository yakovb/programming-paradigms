#lang racket

(define (solve matrix)
  1)

(define (transform matrix)
  1)


;; CONTRACT: process-nested-elements: (A -> B) list-of-list-of-A -> list-of-list-of-B
;;
;; PURPOSE: transform the elements of nested lists from A to B
;; given a function that takes A to B
;;



;; CONTRACT: make-set-of-possible-values: number -> set
;;
;; PURPOSE: make a set of possible values for a cell. If the
;; passed in number is zero, possibilities are 1..9. Otherwise
;; the only possibility is the number that was passed in. 
;;
(define (make-set-of-possible-values elem) 
  (if (zero? elem) 
      (set 1 2 3 4 5 6 7 8 9)
      (set elem)))


;; CONTRACT: cell: set number number symbol boolean -> struct
;;
;; PURPOSE: construct a sudoku cell that contains a list of the cell's possible 
;; values, its row and column number, and the box it belongs to
;;
(struct cell (data row col box singleton-checked?))


;; CONTRACT: make-cell: set number number symbol -> struct
;;
;; PURPOSE: construct a sudoku cell as per default constructor
;; except this version passes a default #f for the singleton-checked? value
;;
(define (make-cell data row col box [singleton-checked? #f])
  (cell data row col box singleton-checked?))


;; CONTRACT: box-lookup: number number -> symbol
;;
;; PURPOSE: take a row and column number and return a symbol that 
;; signifies the box a row/col correspond to. Row options are upper/middle/lower
;; and column options are left/middle/right
;;
(define (box-lookup row col)
  (define (row-lookup r)
    (cond [(<= r 3) "upper"]
          [(<= r 6) "middle"]
          [(<= r 9) "lower"]
          [else (error "box-lookup failed because the supplied row is greater than 9")]))
  
  (define (col-lookup c)
    (cond [(<= c 3) "left"]
          [(<= c 6) "middle"]
          [(<= c 9) "right"]
          [else (error "box-lookup failed because the supplied col is greater than 9")]))
  
  (string->symbol (string-append
                   (row-lookup row)
                   "-"
                   (col-lookup col))))


;; Export of relevant functions for testing purposes
;;
(provide make-set-of-possible-values
         (struct-out cell)
         make-cell
         box-lookup)