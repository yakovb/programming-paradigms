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
  (flatten (go transformed-puzzle 1 empty)))


;; CONTRACT: process-singletons: (A -> Boolean) (A -> A) list-of-A -> list-of-A
;;
;; PURPOSE: partition a list into elems passing and failing the predicate. 
;; Then modify the failing elements using the second passed-in function. Finally,
;; combine and return the passing elements and the modified failing elements
;;
;TODO TRANS should be custom func: remove ? from row? col? box?
;TODO singletons toggle their singleton-checked?
;TODO each cell in singles needs to work on the OTHERS list separately, and consecutively
;output from one being input to the next, and THEN append this to SINGLES list
(define (separate-singletons 
         pred 
         #:func-passes [func-passes identity] 
         #:func-fails [func-fails identity] 
         input) 
  (let-values ([(pass-pred fail-pred) (partition pred input)])
    (append pass-pred (foldr (lambda (elem z) (cons (func-fails elem) z))
                             empty
                             fail-pred))))


;; CONTRACT: valid-singleton?: cell -> Boolean
;;
;; PURPOSE: #t if cell.singleton-checked? is false AND cell.data.size = 1
;;
(define (valid-singleton? cell)
  (and (not (cell-singleton-checked? cell))
       (eq? 1 (set-count (cell-data cell)))))


;; CONTRACT: cell-associations: cell -> list(number number symbol)
;;
;; PURPOSE: extracts the row, column and box associated with a given 
;; cell and returns these as a list
;;
(define (cell-associations c)
  (list (cell-row c) (cell-col c) (cell-box c)))


;; CONTRACT: make-cell-without: cell number -> cell
;;
;; PURPOSE: given a cell and a number, makes a new cell similar to the original
;; except that its set does not contain the number passed as the argument to this function
;;
(define (make-cell-without c num)
  (let ([new-set (set-subtract (cell-data c) (set num))])
    (if (set-empty? new-set)
        (error "FAIL: attempted to make a cell with an empty set as data. Something has gone wrong!")
        (make-cell new-set (cell-row c) (cell-col c) (cell-box c)))))


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
         separate-singletons
         valid-singleton?
         cell-associations
         make-cell-without
         process-nested-elements
         make-set-of-possible-values
         (struct-out cell)
         make-cell
         box-lookup)