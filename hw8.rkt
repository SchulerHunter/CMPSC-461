#lang racket
(define(sumsquare n) (if (= n 0) 0 (sumSquareHelper (- n 1) (* n n))))
(define(sumSquareHelper n sum) (if (= n 0) sum (sumSquareHelper (- n 1) (+ sum (* n n)))))