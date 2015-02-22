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
(define (process-elements func input)
  (foldr (lambda (row z)
           (cons (map (lambda (elem) (func elem)) row) z))
         empty
         input))

; create cell structs with manual recursion
(define (make-cells input row-num result)
  (if (empty? input)
      (reverse result)
      (let ([current-row (first input)]
            [remaining-rows (rest input)]
            [list-1-to-9 (range 1 10)])
        (make-cells
         remaining-rows
         (+ 1 row-num)
         (cons (map (lambda (item col-num) (cell item row-num col-num (box-lookup row-num col-num))) 
                    current-row
                    list-1-to-9)
               result)))))

; lookup table to determine correct box
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
        


          
; test for defining a cell struct
(struct cell (data row col box singleton-checked?))

; helper constuctor which can use default value of #f for singleton-checked?
(define (make-cell data row col box [singleton-checked? #f])
  (cell data row col box singleton-checked?))