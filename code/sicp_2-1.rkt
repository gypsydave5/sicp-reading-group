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
(((add-1 zero) inc) 0) 			;;  1
(((add-1 (add-1 zero)) inc) 0) 		;;  2
(((add-1 (add-1 (add-1 zero))) inc) 0) 	;;  3

(((add-1 (add-1 zero)) ex) '()) 	;;  {() ()}
(((add-1 (add-1 (add-1 zero))) ex) '()) ;;  {() () ()}

(add-1 zero)
(add-1 (lambda (f) (lambda (x) x)))

;; (define one (add-one zero))
;; (define one (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x)))))
(define one (lambda (f) (lambda (x) (f x))))
;; (define two (add-one one))
;; (define two (lambda (f) (lambda (x) (f (((one f) x)))))
;; (define two (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f x))) f) x)))))
(define two (lambda (f) (lambda (x) (f (f x)))))

((two inc) 0) 				;;  2

(define (lambdadd f1 f2)
  (lambda (f) (lambda (x) ((f1 f) ((f2 f) x)))))

(((lambdadd two two) ex) '()) 		                   ;;  {() () () ()}
(((lambdadd two one) inc) 0) 	                           ;;  3
(((lambdadd (lambdadd two two) (lambdadd two one)) inc) 0) ;;  7
(((lambdadd (lambdadd two two) zero) inc) 0)

;; so... "All Church Numerals are functions that take two parameters" (Wikipedia)
;; What happens to these parameters? Well the first one is applied to
;; the second one n-many times - where n is the number we're representing.
;;
;; That's pretty easy isn't it?
;;
;; Call the first parameter `f' (it's a function), and the second one `x' (why not)
;;
;; So what's zero? Easy, take one parameter, take another parameter, and then
;; just do _nothing_ to the second parameter - apply the second parameter 0 times
;;
;; And what's add-one? Bit harder - what are we adding one to? Some number represented
;; as a Church numeral. So the first parameter is another Church numeral
;; - let's call it n. We return a new Church numeral - so it takes two more parameters
;; (f and x). So we just need to do `f' to the number we've been given (`n') one more
;; time in order to `add-one'. So we get to ((n f) x) to get us the original number,
;; then just wrap it in another (f)
;;
;; Man I wish I could write a...
;; zero = λf.λx.x
;; add-one = λn.λf.λx.f (n f x)
;;
;; schweeeeeeeeeeeet

;; multiplication
(define (l-mul n1 n2)
  (lambda (f) (lambda (x) ((n1 (n2 f)) x))))

(((l-mul (l-mul two two) (l-mul two two)) inc) 0) ;;  16

;; decrement
(define (minus-one n)
  (lambda (f) (lambda (x)
                (((n (lambda (g) (lambda (h)
                                   (h (g f))))) (lambda (_) x))
                 (lambda (i) i)))))

;; It's like `n' is now able to receive _3_ arguments instead of the expected two
;;  - this behaviour must be caused by the arguments it's given - specifically
;; (lambda (g) (lambda (h) (h (g f)))) - the `h' parameter is what soaks up the identity.

;; (lambda (f) (lambda (x)
;;               ((((lambda (r) (lambda (s) (r (r s)))) ;; i.e. two
;;                  (lambda (g) (lambda (h) (h (g f)))))
;;                 (lambda (y) x))
;;                (lambda (i) i))))

;; (lambda (f) (lambda (x)
;;               (((lambda (s) ((lambda (g) (lambda (h) (h (g f))))
;;                              ((lambda (g) (lambda (h) (h (g f)))) s)))
;;                 (lambda (y) x))
;;                (lambda (i) i))))

;; (lambda (f) (lambda (x)
;;               (((lambda (g) (lambda (h) (h (g f))))
;;                 ((lambda (g) (lambda (h) (h (g f)))) (lambda (y) x)))
;;                (lambda (i) i))))

;; (lambda (f) (lambda (x)
;;               (((lambda (g) (lambda (h) (h (g f))))
;;                 (lambda (h) (h ((lambda (y) x) f))))
;;                (lambda (i) i))))

;; (lambda (f) (lambda (x)
;;               (((lambda (g) (lambda (h) (h (g f))))
;;                 (lambda (h) (h x)))
;;                (lambda (i) i))))

;; (lambda (f) (lambda (x)
;;               (((lambda (h) (h ((lambda (h) (h x)) f))))
;;                (lambda (i) i))))

;; (lambda (f) (lambda (x)
;;               (((lambda (h) (h (f x))))
;;                (lambda (i) i))))

;; (lambda (f) (lambda (x)
;;               ((lambda (i) i) (f x))))

;; (lambda (f) (lambda (x)
;;               (f x)))


(((minus-one (lambdadd two two)) inc) 0)


;; 2.1.4 Extended Exercise: Interval Arithmetic
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
       (let ((p1 (* (lower-bound x) (lower-bound y)))
             (p2 (* (lower-bound x) (upper-bound y)))
             (p3 (* (upper-bound x) (lower-bound y)))
             (p4 (* (upper-bound x) (upper-bound y))))
         (make-interval (min p1 p2 p3 p4)
                        (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

;; Exercise 2.7
(define (make-interval a b) (cons a b))
(define (upper-bound interval) (cdr interval))
(define (lower-bound interval) (car interval))

;; Exercise 2.8
;; By the argument that the two bounds should always be the most extreme version
;; of the possible result, the lower should have the upper taken away, and vice versa
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;; Exercise 2.9
;; (2 8) - width 3; (4 6) - width 1
(add-interval (make-interval 2 8)
              (make-interval 4 6))      ;;  {6 . 14} = (+ 3 1) =  width 4
(add-interval (make-interval 4 5)
              (make-interval 4 5))      ;;  {8 . 10} = (+ half half) = width 1
(add-interval (make-interval 4 8)
              (make-interval 4 8))      ;;  {8 . 16} = (+ 2 2) = width 4
;; We can think of the width as being the measure of difference, with addition increasing
;; difference (sum of widths), and subtraction...
(sub-interval (make-interval 2 8)
              (make-interval 4 6))      ;;  {-4 . 4} = width 4
(sub-interval (make-interval 2 3)
              (make-interval 2 3))      ;;  {-1 . 1} = (- half half) = width 1
(sub-interval (make-interval 4 8)
              (make-interval 4 8))      ;;  {-4 . 4} = (- 2 2) = width 4
;; width (i.e. difference) sums with both subtraction and addition

(mul-interval (make-interval 4 8)
              (make-interval 4 8))      ;;  {16 . 64} = width 24 ... eh?

;; Exercise 2.10
(define (div-interval-e x y)
  (if (and (> 0 (lower-bound y)) (< 0 (upper-bound y)))
      (error "Divisor spans 0" y)
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

;; Exercise 2.11
(define (mul-interval-2 x y)
  (cond (true )))
