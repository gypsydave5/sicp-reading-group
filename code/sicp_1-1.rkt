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

(define in-range
  (lambda (lower upper)
    (lambda (n) (and (< lower n) (> upper n)))))

;; Exercise 1.2

(/ (+ 5 (+ 4 (- 2 (- 3 (+ 6 (/ 4 5))))))
   (* 3 (* (- 6 2) (- 2 7)))) ;;  -37/150

;; Exercise 1.3
(define (sum-sq-largest a b c)
  (cond
   ((and (> a b) (> c b)) (sum-of-squares a c))
   ((and (> b a) (> c a)) (sum-of-squares b c))
   (else (sum-of-squares a b))))

;; Exercise 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;;; applies a and b to `+` if b > 0, else applies them to `-`

;; Exercise 1.5
(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))

;; 1.1.7

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.0001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;; Exercise 1.6
;; This fails as the else arm of `new-if` will recur infinitely, as it will always be evaluated

;; Exercise 1.7 - Better `good-enough`

(define (better-good-enough? guess last-guess)
  (<
   (abs (- (/ last-guess guess) 1.0)) 	;; absolute difference between guesses
   0.00000001))

(define (sqrt-iter2 guess last-guess x)
  (if (better-good-enough? guess last-guess)
      guess
      (sqrt-iter2 (improve guess x) guess x)))

(define (sqrt2 x)
  (sqrt-iter2 1.0 x x))

;; Exercise 1.8 - Cube Root

(define (cube-root-improve guess x)
  (/ (+ (* 2 guess) (/ x (square guess))) 3))

(define (cube-root-iter guess last-guess x)
  (if (better-good-enough? guess last-guess)
      guess
      (cube-root-iter (cube-root-improve guess x) guess x)))

(define (cube-root x)
  (cube-root-iter 1.0 x x))

;; M1.3 - benefits of special forms
;; Write a Scheme expression whose evaluation would result in an errow if and were a proceedure, but actually will have a value because and is a special form. Do the same for or
(and false (- -))
(or true (3 + 1))

;; M1.4
;; Define a proceedure sign that takes a number as its argument and returns 1 if the number is positive, -1 if the number is negative, and 0 if the number is 0.

(define (sign x)
  (cond ((< 0 x) 1)
	((> 0 x) -1)
	(else 0)))

(define (sign2 x)
  (if (= 0 x)
      0
      (/ x (abs x))))

;; M1.5
;; Define a procedure called true-false that takes one argument and returns 1 if the argument is true and 0 if it is false. For example,
;; (true-false (> 3 2))
;; => 1
;; (true-false (and (> 3 2) (< 3 1)))
;; => 0
;; Write two definitions of true-false – one that uses if and one that uses cond

(define (true-false-cond b)
  (cond (b 1)
	(else 0)))
(define (true-false-if b)
  (if b 1 0))
