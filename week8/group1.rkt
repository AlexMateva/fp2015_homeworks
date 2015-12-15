#lang racket
; zad 1

(define (sum-divisors n)
  (define (sum-iter i result)
    (cond [(> i n) result]
          [(= (remainder n i) 0) (sum-iter (+ i 1) (+ result i))]
          [else (sum-iter (+ i 1) result)])
  )
  (sum-iter 1 0)
)

(define (prime? n)
  (= (sum-divisors n) (+ n 1)))
  
(define (truncatable-prime? x)
  (cond
    [(< x 10) (prime? x)]
    [(prime? x) (truncatable-prime? (quotient x 10))]
    [else #f]))

; zad 2

(define (where list-elements list-predicates)
  (cond
    [(empty? list-elements) (list)]
    [(empty? list-predicates) list-elements]
    [else (where (filter (first list-predicates) list-elements) (rest list-predicates))]))

; zad 3

(define (to-zero i lst)
  (define (help j left right)
    (cond
      [(= j i) (append left (list 0) (rest right))]
      [else (help (+ j 1) (append left (list (first right))) (rest right))]))
  (help 0 (list) lst))

(define (zeroes matrix)
  (define (help i res mat row)
    (cond
      [(empty? row) (if (empty? mat) res (help 0 res (rest mat) (first mat)))]
      [(= (first row) 0) (help (+ i 1) (cons i res) mat (rest row))]
      [else (help (+ i 1) res mat (rest row))]))
  (help 0 (list) (rest matrix) (first matrix)))

(define (zeroed i matrix)
  (cond
    [(empty? matrix) (list)]
    [else (cons (to-zero i (first matrix)) (zeroed i (rest matrix)))]))

(define (zero matrix)
  (define (cycle lst mat)
    (cond
      [(empty? lst) mat]
      [else (cycle (rest lst) (zeroed (first lst) mat))]))
  (let
      ([zeroes (zeroes matrix)])
      (cycle zeroes matrix)))

; zad 4

(define (i-th-column i M)
  (define (help j res mat row)
    (cond
      [(empty? row) (if (empty? mat)
                        res
                        (help 0 res (rest mat) (first mat)))]
      [(= j i) (if (empty? mat)
                   (cons (first row) res)
                   (help 0 (cons (first row) res) (rest mat) (first mat)))]
      [else (help (+ j 1) res mat (rest row))]))
  (reverse (help 0 (list) (rest M) (first M))))

(define (main-diag M)
  (define (help i j res mat row)
    (cond
      [(empty? row) res]
      [(= i j) (if (empty? mat)
                   (cons (first row) res)
                   (help (+ i 1) 0 (cons (first row) res) (rest mat) (first mat)))]
      [else (help i (+ j 1) res mat (rest row))]))
  (reverse (help 0 0 (list) (rest M) (first M))))

(define (secondary-diag M)
  (define (help i j res mat row)
    (cond
      [(empty? row) res]
      [(= (+ i j) (- (length M) 1)) (if (empty? mat)
                                       (cons (first row) res)
                                       (help (+ i 1) 0 (cons (first row) res) (rest mat) (first mat)))]
      [else (help i (+ j 1) res mat (rest row))]))
  (reverse (help 0 0 (list) (rest M) (first M))))

(define (rows? M)
  (cond
    [(= (length M) 1) #t]
    [(= (foldl + 0 (first M)) (foldl + 0 (first (rest M)))) (rows? (rest M))]
    [else #f]))

(define (columns? M)
  (define (help i)
    (cond
      [(= i (- (length M) 1)) #t]
      [(= (foldl + 0 (i-th-column i M)) (foldl + 0 (i-th-column (+ i 1) M))) (help (+ i 1))]
      [else #f]))
  (help 0))

(define (magic-square? M)
  (and (rows? M)
       (columns? M)
       (= (foldl + 0 (i-th-column 0 M)) (foldl + 0 (main-diag M)))
       (= (foldl + 0 (i-th-column 0 M)) (foldl + 0 (secondary-diag M)))))
; zad 5

(define (string-repeat str n)
  (define (help i res)
    (if (= i n)
        res
        (help (+ i 1) (string-append res str))))
  (help 0 ""))

(define (repeater str)
  (lambda (count glue)
    (string-append str (string-repeat (string-append glue str) (- count 1)))))
