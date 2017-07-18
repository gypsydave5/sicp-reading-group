#lang sicp
(#%require (rename racket racket-random random))
(#%require (planet williams/science/random-source))
(define (random n)
  (if (and (exact? n) (integer? n))
      (random-integer n)
      (* n (racket-random))))

(define (cube n)
  (* n n n))

(define (identity x) x)

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (sum-integers a b)
  (sum identity a inc b))

(define (pi-sum a b)
  (define (term n)
    (/ 1.0 (* n (+ a 2))))
  (define (next n)
    (+ 4 n))
  (sum term a next b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

;; Exercise 1.29

(define (simpsons-rule f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (coeff k) (if (even? k) 2 4))
  (define (term k) (* (coeff k) (y k)))
  (* (/ h 3)
     (+ (y 0) (y n) (sum term 1 inc (dec n)))))

;; Exercise 1.30
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (+ result (term a)))))
  (iter a 0))
(define (sum-integers-iter a b)
  (sum-iter identity a inc b))

;; Exercise 1.31
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (* result (term a)))))
  (iter a 1))

(define (factorial n)
  (product identity 1 inc n))
