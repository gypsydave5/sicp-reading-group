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

;; 2.1.3 What is Meant by Data?

;; what makes the above a representation of rational numbers?
;; For all integers n, and non zero integer d, if x is `(make-rat n d)` then (= (numer x) n)
;; and (= (denom x) d)

;; the same for cons, car and cdr

;; and we can create these data structures out of procedures...

(define (cons1 x y)
  (define (dispatch m)
    (cond ((= m 0) x)
	  ((= m 1) y)
	  (else (error "Argument not 0 or 1 -- CONS" m))))
  dispatch)

(define (car1 z) (z 0))
(define (cdr1 z) (z 1))

;; This is indistinguishable from the "real" data structure of cons. The procedural representation
;; is perfectly adequate as it does the three things it needs to do...
;; ...

;; Hey, this is interesting

(define (new-map)
  (define (no-such-key k)
    (error "No such key in map:" k))
  no-such-key)

(define (add map key value)
  (define (new-map k)
    (if (equal? k key)
	value
	(map k)))
  new-map)

(define (get map key)
  (map key))

(define my-map (add (new-map) 'a 1))
(define my-map-2 (add my-map 'b 2))
(define my-map-3 (add my-map-2 'c 3))
(get my-map-3 'a)
(get my-map-3 'b)
(get my-map-3 'c)

;; OK enough of this...

;; Exercise 2.4 - even more cons!
;; (this is even better)

(define (consp x y)
  (lambda (m) (m x y)))

(define (carp z)
  (z (lambda (p q) p)))

(define (cdrp z)
  (z (lambda (p q) q)))

(carp (consp 1 2))
(cdrp (consp 'a 'b))

;; Exercise 2.5
(define (consa a b)
  (* (expt 2 a) (expt 3 b)))

(define (c*ra base)
  (define (iter c count)
    (if (not (integer? (/ c base)))
	count
	(iter (/ c base) (+ 1 count))))
  (lambda (c) (iter c 0)))

(define cara (c*ra 2))
(define cdra (c*ra 3))

(cara (consa 55 88))
(cdra (consa 128 256))

;; data can be encoded in numbers :D

;; Exercise 2.6

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define (inc n) (+ 1 n))
(define (ex l) (cons '() l))

;; e.g.
(((add-1 zero) inc) 0)
(((add-1 (add-1 zero)) inc) 0)
(((add-1 (add-1 (add-1 zero))) inc) 0)

(((add-1 (add-1 zero)) ex) '())
(((add-1 (add-1 (add-1 zero))) ex) '())
