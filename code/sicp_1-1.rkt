#lang sicp

;; utils
(define (println s)
  (display s)
  (newline))

;; 1.1.4

(define square
  (lambda
      (x)
    (* x x)))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

;; 1.1.6

(define (abs-1 x)
  (cond ((> x 0) x)
	((< x 0) (- x))))

(define (abs-2 x)
  (cond ((< x 0) (- x))
	(else x)))

(define (abs-3 x)
  (if (< x 0)
      (- x)
      x))
