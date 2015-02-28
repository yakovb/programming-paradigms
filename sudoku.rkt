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


;; CONTRACT: reduce-singletons: list-of-cells -> list-of-cells
;;
;; PURPOSE: take a list of cells, determine the singletons, and remove the 
;; singleton-values from the associated rows, columns and boxes, returning
;; the resulting list of modified cells
;;
(define (reduce-singletons input)
  (let-values ([(singles others) (partition valid-singleton? input)])    
    (if (empty? singles)
        #f
        (append (toggle-checked-singletons singles) 
                (remove-from-associated singles others)))))


;; CONTRACT: find-single-val-in-set: list-of-cells -> list-of-celss
;;
;; PURPOSE: given a list of cells, remove existing singletons and find the cells in the 
;; remaining list that contain numbers not occuring in the same row/cell/box (ie can be 
;; made into a singleton set). Turn these cells into singletons, rejoin them and their 
;; associations onto the existing singletons and return the updated list of cells
;;
(define (find-single-val-in-set input)       
  (let-values ([(singles candidates) (partition valid-singleton? input)])
    (append singles (singles-in-candidate-cells candidates))))
   

;; CONTRACT: singles-in-candidate-cells: list-of-cells -> list-of-cells boolean
;;
;; PURPOSE: in all cells that aren't already singletons, find all the cells which can be 
;; reduced to singletons and make them so. If any changes are made, return the new list of
;; cells along with a #t flag, otherwise return the original input with a #f flag
;;
(define (singles-in-candidate-cells input)
  
  (define (go input n change-flag)
    (if (= n (length input))
        (values input change-flag)
        (let*-values ([(associated others) (partition (lambda (cell) (associated-cells? (first input)
                                                                                        cell))
                                                      input)]
                     [(new-associated result-flag) (singles-in-associated-cells associated)]
                     [(rejoined) (append new-associated others)]
                     [(new-input) (to-end-of-list (first rejoined) (rest rejoined))])
          (go new-input (+ n 1) (and change-flag result-flag)))))
  
    (go input 0 #f))


;; CONTRACT: singles-in-associated-cells: list-of-cells -> list-of-cells boolean
;;
;; PURPOSE: finds all the cells which have a number not occuring elsewhere in their associated
;; cells. Returns these cells as singleton cells along with their associated cells, along with #t
;; if any changes have been made. Otherwise return the original input with #f to signify that no
;; changes have been made 
;;
(define (singles-in-associated-cells input)
  
  (define (go input n change-flag)
    (if (= n (length input))
        (values input change-flag)
        (let*-values ([(new-single result-flag) (make-single-if-poss (first input) (rest input))]
                      [(new-input) (to-end-of-list new-single (rest input))])
          (go new-input (+ n 1) (and change-flag result-flag)))))
  
  (go input 0 #f))


;; CONTRACT: to-end-of-list: A list-of-A -> A list-of-A
;;
;; PURPOSE: given an item and a list, return a list where the last item is now
;; the previous head, and the first item is now the previous second item
;
(define (to-end-of-list head tail)
  (cons (first tail)
        (append (rest tail) (list head))))
                  
          
;; CONTRACT: valid-singleton?: cell -> boolean
;;
;; PURPOSE: #t if cell.singleton-checked? is false AND cell.data.size = 1
;;
(define (valid-singleton? cell)
  (and (not (cell-singleton-checked? cell))
       (eq? 1 (set-count (cell-data cell)))))


;; CONTRACT: found-single-num: cell list-of-cells -> cell boolean
;;
;; PURPOSE: determine whether there is a number in cell that does not occur in the 
;; list-of-cells. If true, then remake the cell as a singleton and return it along with #t
;; to signify that a change has been made. Otherwise return the original cell along with #f
;; to signify that no change has been made
;;
(define (make-single-if-poss test-cell test-subjects)
  (let* ([result (for/lists (result)
                  ([num (cell-data test-cell)])
                  (if (not (member num
                                   (flatten (map (lambda (c) (set->list (cell-data c))) test-subjects))))
                      (make-singleton test-cell num)
                      #f))]
         [found (filter (lambda (r) (not (false? r))) result)])
    (if (= 1 (length found))
        (values (first found) #t)
        (values test-cell #f))))
         

;; CONTRACT: remove-from-associated: list-of-cells list-of-cells -> list-of-cells
;;
;; PURPOSE: take a list of singletons and remove their cell values from each
;; of their associated cells, returning the new, thinned, list of cells
;;
(define (remove-from-associated singles others)
  (if (empty? singles)
      others
      (remove-from-associated (rest singles)
                              (let ([single (first singles)])
                                (map (lambda (other) (if (associated-cells? other single)
                                                         (make-cell-without (cell-data single) other)
                                                         other))
                                     others)))))


;; CONTRACT: associated-cells?: cell cell -> boolean
;;
;; PURPOSE: determines whether two cells share a row, column, or box
;;
(define (associated-cells? c1 c2)
  (let ([c1-assoc (cell-associations c1)]
        [c2-assoc (cell-associations c2)])
    (ormap equal? c1-assoc c2-assoc)))


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
(define (make-cell-without num c)
  (let ([new-set (set-subtract (cell-data c) num)])
    (if (set-empty? new-set)
        (error "FAIL: attempted to make a cell with an empty set as data. Something has gone wrong!")
        (make-cell new-set (cell-row c) (cell-col c) (cell-box c)))))


;; CONTRACT: make-singleton: cell number -> cell
;;
;; PURPOSE: given a cell and a number, make a new cell that only has the number
;; in its set of possibilities, with all other cell member variables the same
;;
(define (make-singleton c num)
  (if (set-member? (cell-data c) num)
      (make-cell (set num) (cell-row c) (cell-col c) (cell-box c))
      (error "supplied singleton value is not possible in this cell")))


;; CONTRACT: toggle-checked-singletons: list-of-cells -> list-of-cells
;;
;; PURPOSE: once a singleton cell has had its data removed from associated
;; rows, columns and boxes, it is marked as checked
;;
(define (toggle-checked-singletons singles)
  (map (lambda (c) 
         (make-cell (cell-data c) (cell-row c) (cell-col c) (cell-box c) #t))
       singles))

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
         reduce-singletons
         singles-in-candidate-cells
         singles-in-associated-cells
         valid-singleton?
         make-single-if-poss
         associated-cells?
         cell-associations
         make-cell-without
         make-singleton
         toggle-checked-singletons
         process-nested-elements
         make-set-of-possible-values
         (struct-out cell)
         make-cell
         box-lookup)