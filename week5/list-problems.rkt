#lang racket
(define (sum numbers)
    (cond
    [(empty? numbers ) 0]
    [else (+ (first numbers) (sum (rest numbers)))]))
;;;;;;;;;;;;;;;;;;;;;;;;;
(define (member? x items)
   (cond
    [(empty? items ) #f]
    [(equal? (first items) x) #t]
    [else (member x (rest items))]))
;;;;;;;;;;;;;;;;;;;;;;;;;

(define (range2 a b)
  (cond
    [(= a (- b 1)) (list a)]
    [else (cons a (range2 (add1 a) b))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (length2 numbers)
(define (iter numbers r)
  (cond
    [(empty? numbers) r]
    [else (iter (rest numbers) (+ r 1))]))
  (iter numbers 0))
;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (list-ref2 items n)
  (cond
    [(zero? n) (first items)]
    [else (list-ref2 (rest items) (- n 1))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (build-list2 n f)
 (define (iter i res)
    (if (< i 0)
        res
        (iter (- i 1) (cons (f i) res))))
  (iter (- n 1) (list))
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (append2 l1 l2)
  (define (helper result xs)
    (if (empty? xs)
        result
        (helper (cons (first xs) result) (rest xs))))
  (helper l2 l1)
  )
;;;;;;;;;;;;;;;;;;;;;;;;;

(define (reverse2 items)
  (define (help res lst)
    (if (empty? lst)
        res
        (help (cons (first lst) res) (rest lst))))
  (help  (list) items)
  )
;;;;;;;;;;;;;;;;;;;;;;;;;

(define (take2 n items)
  (define (helper i result lst)
    (if (= i n)
        result
        (helper (+ i 1) (cons (first lst) result) (rest lst))))
  (if (> n (length items))
      items
      (reverse2 (helper 0 (list) items)))
  )
;;;;;;;;;;;;;;;;;;;;;;;;

(define (drop2 n items)
  (define (helper i result)
    (if (= i n)
        result
        (helper (+ i 1) (rest result))))
  (if (> n (length items))
      (list)
      (helper 0 items))
  )
;;;;;;;;;;;;;;;;;;;;;;;

(define (take-while p items)
(define (helper result lst)
  (cond
    [(empty? lst) result]
    [(not (p (first lst))) result]
    [else (helper (cons (first lst) result) (rest lst))]))
  (reverse2 (helper (list) items)))
;;;;;;;;;;;;;;;;;;;;;;;

