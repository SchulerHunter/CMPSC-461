#lang racket
(define TRUE (lambda (x) (lambda (y) x)))
(define FALSE (lambda (x) (lambda (y) y)))
(define (IF b tv fv) ((b tv) fv))

(define (DECODEBOOL b) ((b #t) #f))

(define (ENCODE n)
  (if (= n 0)
      (lambda (f) (lambda (x) x))
      (lambda (f) (lambda (x) (f (((ENCODE (- n 1)) f) x))))))

(define ZERO (ENCODE 0))

(define (DECODE n) ((n add1) 0))

(define (SUCC n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
(define (PLUS n1 n2)
  ((n1 SUCC) n2))

(define (PAIR v1 v2) (lambda (f) (f v1 v2)))
(define (LEFT p) (p (lambda (v1 v2) v1)))
(define (RIGHT p) (p (lambda (v1 v2) v2)))

;;; Start implementing your functions below

(define (ISZERO n) ((n (lambda (x) FALSE)) TRUE))

(define (PRED n) (RIGHT ((n (lambda (p) (PAIR (SUCC (LEFT p)) (LEFT p)))) (PAIR ZERO ZERO))))

(define (MINUS n1 n2) ((n2 PRED) n1))

(define (sequence low high fac) (if (<= low high) (cons low (sequence (* low fac) high fac)) null))

(define (sum lst) (foldl + 0 lst))

(define (sqrList lst) (map (lambda (x) (* x x)) lst))
(define (test lst) (foldl cons null lst))
(define (sizeOf t) (if (list? t) (foldl (lambda (x ret) (+ ret (sizeOf x))) 0 t) 1))