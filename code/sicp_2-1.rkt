#lang sicp
(#%require "helpers.rkt")

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

;; (define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "\\")
  (display (denom x)))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(define (make-rat-2 n d)
  (let ((g (gcd (abs n) (abs d))))
    (cond ((and (negative? n) (negative? d))
	   (cons (/ (abs n) g) (/ (abs d) g)))
	  ((or (negative? n) (negative? d))
	   (cons (/ (- (abs n)) g) (/ (abs d) g)))
	  (else
	   (cons (/ (abs n) g) (/ (abs d) g))))))


;; Exercise 2.2

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment point-one point-two)
  (cons point-one point-two))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))

(define (make-point x y)
  (cons x y))
(define (x-point point)
  (car point))
(define (y-point point)
  (cdr point))

(define (midpoint-segment segment)
  (let ((start (start-segment segment))
	(end (end-segment segment)))
    (make-point (average (x-point start) (x-point end))
		(average (y-point start) (y-point end)))))

;; Exercise 2.3

;; first this
(define (make-rect point-one point-two)
  (cons point-one point-two))

;; eventually-these-two
(define (first-corner-rect rect)
  (car rect))

(define (second-corner-rect rect)
  (cdr rect))

;; Then I looked to define width and height
(define (width-rect rect)
  (abs (- (x-point (first-corner-rect rect))
	  (x-point (second-corner-rect rect)))))

(define (height-rect rect)
  (abs (- (y-point (first-corner-rect rect))
	  (y-point (second-corner-rect rect)))))

;; I defined these two first
(define (perimeter-rect rect)
  (+ (* 2 (width-rect rect))
     (* 2 (height-rect rect))))

(define (area-rect rect)
  (* (width-rect rect)
     (height-rect rect)))

;; second definition
(define (make-rect2 width height)
  (cons width height))

;; key abstraction barrier
(define (width-rect2 rect)
  (car rect))

(define (height-rect2 rect)
  (cdr rect))

;; same two functions
(define (area-rect2 rect)
  (* (width-rect2 rect)
     (height-rect2 rect)))

(define (perimeter-rect2 rect)
  (+ (* 2 (width-rect2 rect))
     (* 2 (height-rect2 rect))))
