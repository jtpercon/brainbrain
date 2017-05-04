#lang racket
(require srfi/48)

(define height 12) ; number of rows. must be positive
(define width 12) ; number of cols. must be greater than 4

(define start-row (quotient height 2)) ; start and end halfway down the map
(define end-col (- width 2)) ; go from left to right end of map

; grid API
(define starting-grid
  (list-update (make-list height (make-list width #\.))
               start-row
               (λ (row) (append '(#\- #\- #\-) (drop-right (drop row 3) 1) '(#\-)))))
(define (blank-grid x y) (make-list y (make-list x #\.)))
(define (grid-ref grid x y) (list-ref (list-ref grid y) x))
(define (grid-update grid x y v)
  (list-update grid y (λ (row) (list-update row x (λ (_) v)))))
(define (tile->string tile)
  (if (number? tile)
      (number->string tile)
      (string tile)))
(define (row->string row)
  (if (empty? row)
      ""
      (string-append (format "~2F " (tile->string (first row))) (row->string (rest row)))))
(define (grid->string grid)
  (if (empty? grid)
      ""
      (string-append (row->string (first grid)) (string #\newline) (grid->string (rest grid)))))

; path is denoted by ascending whole numbers in the grid
; spaces not along the path have the character they will have in the level map


(define (adjacent-tiles i j)
  (filter (λ (item) (and (pair? item) (<= 0 (car item)) (< (car item) width) (<= 0 (cdr item)) (< (cdr item) height)))
          (list (cons (add1 i) j) (cons (sub1 i) j) (cons i (add1 j)) (cons i (sub1 j)))))


(define (make-path grid x y n)
  (if (and (= x end-col) (= y start-row))
      (grid-update grid x y n)
      (if (eq? (grid-ref grid x y) #\.)
          (try-path (shuffle (adjacent-tiles x y))
                    (grid-update grid x y n)
                    x y (add1 n))
          #f)))

(define (try-path paths grid x y n)
  (if (empty? paths)
      #f
      (let ([first-path (make-path grid (car (first paths)) (cdr (first paths)) n)])
        (if first-path
            first-path
            (try-path (rest paths) grid x y n)))))

; takes a point on the grid and returns a list of the places on the track it is adjacent to
(define (nearby-track grid x y)
  (if (eq? (grid-ref grid x y) #\.)
      (filter number? (map (λ (p) (grid-ref grid (car p) (cdr p))) (adjacent-tiles x y)))
      empty))

(define (gather-candidates grid)
  (define (helper x y)
    (if (= y height) empty
        (let ([tile-candidates (cons (cons x y) (nearby-track grid x y))]
              [next-x (if (= (add1 x) width) 0 (add1 x))]
              [next-y (if (= (add1 x) width) (add1 y) y)])
        (cons tile-candidates (helper next-x next-y)))))
  (filter (λ (l) (not (empty? (cdr l)))) (helper 0 0)))
     

  
(define level (make-path starting-grid 3 start-row 0))

(display (grid->string level))
(length (gather-candidates level))