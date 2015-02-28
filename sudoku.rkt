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


;; CONTRACT: locate-singletons: list-of-cells -> list-of-celss
;;
;; PURPOSE: given a list of cells, remove existing singletons and find a cell in the 
;; remaining list that contains a number not occuring in the same row/cell/box. 
;; Turn this cell into a singleton, rejoin it onto the full list of cells  and return 
;; the updated list
;;
(define (locate-singletons input)
  (define (loopy fst rst i)
    (if (= i (+ 1 (length rst)))
        #f
        (let ([new-single (found-single-num fst rst)])
          (if new-single
              (cons (make-cell (set new-single) (cell-row fst) (cell-col fst) (cell-box fst))
                    rst)
              (loopy (first rst) (append (rest rst) (list fst)) (+ 1 i))))))
  
  (define (found-single-num fst rst)
    (for*/or ([num fst]
              [c rst])
      (if (set-member? (cell-data c) num)
          num
          #f)))
  
  (let*-values ([(checked candidates) (partition valid-singleton? input)]
                [(c-first) (first candidates)]
                [(c-rest) (rest candidates)]
                [(associated others) (partition (lambda (cell) (associated-cells? c-first cell))
                                                c-rest)])
    (let ([result (loopy c-first associated 0)])
      (if result
          (append result checked others)
          #f))))
                  
          


;; CONTRACT: valid-singleton?: cell -> Boolean
;;
;; PURPOSE: #t if cell.singleton-checked? is false AND cell.data.size = 1
;;
(define (valid-singleton? cell)
  (and (not (cell-singleton-checked? cell))
       (eq? 1 (set-count (cell-data cell)))))


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
    (for/or ([elem c1-assoc])
      (member elem c2-assoc))))


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
         valid-singleton?
         associated-cells?
         cell-associations
         make-cell-without
         toggle-checked-singletons
         process-nested-elements
         make-set-of-possible-values
         (struct-out cell)
         make-cell
         box-lookup)