#lang racket

;; CONTRACT: solve: list-of-list-of-number -> list-of-list-of-number
;;
;; PURPOSE: take a sudoku puzzle as a matrix of numbers and return as a matrix either 
;; (a) the solved puzzle with each cell showing its appropriate number, or 
;; (b) the part-solved puzzle with unsolved locations showing a set of possible numbers
;;
(define (solve matrix)                 

  (define (loop cells-list flag)
    (if flag
        (let-values ([(new-list new-flag) (reduce-singletons cells-list)])
            (loop new-list new-flag))
        (let-values ([(new-list new-flag) (find-single-set cells-list)])
          (if new-flag
              (loop new-list new-flag)
              (transform-back new-list)))))
  
  (let ([worklist ((compose1 cells-list transform) matrix)])
    (loop worklist #t)))
  

;; CONTRACT: transform: list-of-list-of-number -> list-of-list-of-sets
;;
;; PURPOSE: take a sudoku puzzle as a matrix; for each number other than 0
;; transform to set containing only that number; for each 0 transform to 
;; set containing numbers 1 through 9
;;
(define (transform matrix)
  (process-nested-elements  make-set-of-possible-values  matrix))


;; CONTRACT: transform-back: list-of-cells -> list-of-list-of-numberORset
;;
;; PURPOSE: takes a list of cells and transforms them back into a list of list
;; of numbers, with each cell in its proper row/column position. If a cell contains 
;; a singleton set, it is represented in the final result as a number; otherwise 
;; it is represented as a set
;;
(define (transform-back cells [n 9])
  
  (define (go output n)
    (if (< n 1)
        output
        (go (cons (do-row cells n) output) (- n 1))))
          
  (define (do-row input row-num)
    (let ([row (filter (lambda (c) (= row-num (cell-row c))) input)])
      (sort row < #:key (lambda (c) (cell-col c)))))
  
  (let ([result (go empty n)])
    (process-nested-elements (lambda (cell) (if (cell-singleton-checked? cell)
                                                (set-first (cell-data cell))
                                                (cell-data cell)))
                             result)))
                                                


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
        (values input #f)
        (values (append (toggle-checked-singletons singles) 
                        (remove-from-associated singles others))
                #t))))


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


;; CONTRACT: find-single-set: list-of-cells -> list-of-cells boolean
;;
;; PURPOSE: take a list of cells and search for a set which has a value not occuring in 
;; its associated row OR column OR box. If found, reduce that set to a singleton and immediately
;; return the modified list-of-cells and #t. If no such set is found, return the unmodified 
;; list-of-cells and #f
;;
(define (find-single-set cells)
(let ([procs (list cell-row cell-col cell-box)])
          
  (define (loop input n flag)
    (if (= n (length input))
        (values input #f)
        (let ([head (first input)]
              [tail (rest input)])
          (if (= 1 (set-count (cell-data head)))
              (loop (to-end-of-list head tail) (+ n 1) flag)
              (let ([setValOrFalse (found procs head tail)])
                (if setValOrFalse
                    (values (cons (make-singleton head setValOrFalse) tail) #t)
                    (loop (to-end-of-list head tail) (+ n 1) flag)))))))                  
                           
  (define (found fs candidate others)
    (if (empty? fs)
        #f
        (let* ([f (first fs)]
               [cand-val (f candidate)]
               [other-vals (filter (lambda (val) (eq? cand-val (f val))) others)]
               [result (for/first ([i (set->list (cell-data candidate))]
                                   #:when (not (member i (flatten (map (lambda (c) (set->list (cell-data c)))
                                                                       other-vals)))))
                         i)])
          (if result
              result
              (found (rest fs) candidate others)))))
  
  (loop cells 0 #f)))
                 

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
;; PURPOSE: determines whether two cells share a row, column, or box. 
;; Will not perform the checking if the cell in question is already a 
;; checked singleton (because such a cell needs no further processing
;;
(define (associated-cells? c1 c2)
  (if (or (cell-singleton-checked? c1)
          (= 1 (set-count (cell-data c1))))
      #f
      (let ([c1-assoc (cell-associations c1)]
            [c2-assoc (cell-associations c2)])
        (ormap equal? c1-assoc c2-assoc))))


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
         (cell (cell-data c) (cell-row c) (cell-col c) (cell-box c) #t))
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

(define puzzle '((0 2 5 0 0 1 0 0 0)
                (1 0 4 2 5 0 0 0 0)
                (0 0 6 0 0 4 2 1 0)
                (0 5 0 0 0 0 3 2 0)
                (6 0 0 0 2 0 0 0 9)
                (0 8 7 0 0 0 0 6 0)
                (0 9 1 5 0 0 6 0 0)
                (0 0 0 0 7 8 1 0 3)
                (0 0 0 6 0 0 5 9 0)))


;; Export of relevant functions for testing purposes
;;
(provide transform
         transform-back
         cells-list
         reduce-singletons
         valid-singleton?
         find-single-set
         remove-from-associated
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