#lang racket


(define y 12) ; number of rows. must be positive
(define x 12) ; number of cols. must be greater than 4

(define start-row (quotient y 2)) ; start and end halfway down the map
(define end-col (- x 2)) ; go from left to right end of map

; path is denoted by ascending whole numbers in the grid
; spaces not along the path have the character they will have in the level map
(define starting-grid
  (list-update (make-list y (make-list x #\.))
               start-row
               (λ (row) (append '(#\- #\- #\-) (drop-right (drop row 3) 1) '(#\-)))))
  
(define (possible-steps i j)
  (filter (λ (item) (and (pair? item) (<= 0 (car item)) (< (car item) x) (<= 0 (cdr item)) (< (cdr item) y)))
          (list (cons (add1 i) j) (cons (sub1 i) j) (cons i (add1 j)) (cons i (sub1 j)))))
        
(define (make-path grid x y n)
  (if (and (= x end-col) (= y start-row))
      (list-update grid y (λ (row) (list-update row x (λ (_) n))))
      (if (eq? (list-ref (list-ref grid y) x) #\.)
          (try-path (shuffle (possible-steps x y))
                    (list-update grid y (λ (row) (list-update row x (λ (_) n))))
                    x y (add1 n))
          #f)))

(define (try-path paths grid x y n)
  (if (empty? paths)
      #f
      (let ([first-path (make-path grid (car (first paths)) (cdr (first paths)) n)])
        (if first-path
            first-path
            (try-path (rest paths) grid x y n)))))

(pretty-print (make-path starting-grid 3 start-row 0))
