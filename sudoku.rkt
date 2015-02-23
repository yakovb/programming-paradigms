#lang racket

(define (solve matrix)
  1)


;; CONTRACT: transform: list-of-list-of-number -> list-of-list-of-sets
;;
;; PURPOSE: take a sudoku puzzle as a matrix; for each number other than 0
;; transform to set containing only that number; for each 0 transform to 
;; set containing numbers 1 through 9
;;
(define (transform matrix)
  (process-nested-elements  make-set-of-possible-values  matrix))


;; CONTRACT: cells-list: list-of-list-of-sets -> list-of-cell-structs
;;
;; PURPOSE: takes a transformed sudoku puzzle and creates a flat list of 
;; cell structs to be used for further processing by other functions
;;
(define (cells-list transformed-puzzle)
  (define (go input row-num result)
    (if (empty? input)
        result
        (let ([this-row (first input)]
              [other-rows (rest input)]
              [col-nums (range 1 10)])
          (go other-rows (+ 1 row-num)
              (cons (map (lambda (elem col-num)
                           (make-cell elem row-num col-num (box-lookup row-num col-num)))
                         this-row
                         col-nums)
                    result)))))
  (flatten (go transformed-puzzle 0 empty)))


;; CONTRACT: process-nested-elements: (A -> B) list-of-list-of-A -> list-of-list-of-B
;;
;; PURPOSE: transform the elements of nested lists from A to B
;; given a function that takes A to B
;;
(define (process-nested-elements func input)
  (foldr (lambda (row z)
           (cons (map (lambda (elem) (func elem)) row) z))
         empty
         input))


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
;; and column options are left/middle/right. The result is in reverse row order
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
(provide transform
         cells-list
         process-nested-elements
         make-set-of-possible-values
         (struct-out cell)
         make-cell
         box-lookup)