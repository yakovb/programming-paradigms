#lang racket

(define (solve matrix)
  1)

(define (transform matrix)
  1)


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

(provide make-set-of-possible-values
         (struct-out cell)
         make-cell)