#lang sicp
(#%require "sicp_1-2.rkt")
(#%require "helpers.rkt")

(define (cube x)
  (* x x x))

(define (square x)
  (* x x))

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
;; a.
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (* result (term a)))))
  (iter a 1))

(define (factorial n)
  (product identity 1 inc n))

(define (pi accuracy)
  (define (term n)
    (cond ((even? n) (/ (+ n 2.0) (+ n 3.0)))
	  (else (/ (+ n 3) (+ n 2)))))
  (* (product term 0 inc accuracy)
     4))

;; b.
(define (product-lr term a next b)
  (if (> a b)
      1
      (* (term a)
	 (product term (next a) next b))))

;; Exercise 1.32
;; a.
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (sum-ac term a next b)
  (accumulate + 0 term a next b))

(define (product-ac term a next b)
  (accumulate * 1 term a next b))

(define (factorial-ac n)
  (product-ac identity 1 inc n))

(define (sum-cubes-ac a b)
  (sum-ac cube a inc b))

;; b.
(define (accumulate-lr combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
		(accumulate combiner null-value term (next a) next b))))

;; Exercise 1.33

(define (filtered-accumulate filter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (combiner result (if (filter a) (term a) null-value)))))
  (iter a null-value))

;; a.
(define (sum-square-primes a b)
  (filtered-accumulate prime? + 0 square a inc b))

;; b.
(define (product-relative-primes n)
  (define (relative-prime x)
    (= 1 (gcd x n)))
  (filtered-accumulate relative-prime * 1 identity 0 inc n))

;; 1.3.2 Construction Procedures Using 'Lambda'

;; let is syntastic sugar for a lambda binding:
(define (f-lambda x y)
  ((lambda (a b)
     (+ (* x (square a))
	(* y b)
	(* a b)))
   (+ 1 (* x y))
   (- 1 y)))

(define (f-let x y)
  (let ((a (+ 1 (* x y)))
	(b (- 1 y)))
    (+ (* x (square a))
	(* y b)
	(* a b))))

;; not like Clojure lets - cannot access previous binding pair... i.e.

(define (let-demo)
  (define x 55)
  (println x)
  (let ((x 5)
	(y (+ 4 x)))
    (println y)
    (println x)
    (+ x y)))

;; Exercise 1.34
(define (f g)
  (g 2))

;; (f f)
;; will expand to (f (f 2))
;; will expand to (f (f (2 2)))
;; will freak out and die

;; 1.3.3 Procedures as General Methods

(define (close-enough? x y)
  (< (abs (- x y)) 0.0000001))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
	midpoint
	(let ((test-value (f midpoint)))
	  (cond ((positive? test-value)
		 (search f neg-point midpoint))
		((negative? test-value)
		 (search f midpoint pos-point))
		(else midpoint))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
	(b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
	   (search f a b))
	  ((and (negative? b-value) (positive? a-value))
	   (search f b a))
	  (else
	   (error "Values are not of opposite sign" a b)))))

;; pi
(define pi-by-sin (half-interval-method sin 2.0 4.0))
;; solution of x^3 - 2x - 3
(define equation-solver (half-interval-method
			 (lambda (x) (- (* x x x) (* 2 x) 3))
			 1.0
			 2.0))

;;; fixed points - f(x) = x

(define tolerance 0.0001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

;; (fixed-point cos 1.0)
;; (fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)
;; Equation y = sin y + cos y

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

;; Exercise 1.35
;; phi^2 = phi + 1
;; phi = (/ (+ phi 1) phi)
;; phi = (+ (/ phi phi) (/ 1 phi))
;; phi = (+ 1 (/ 1 phi))

(define phi (fixed-point (lambda (y) (average y (+ 1 (/ 1 y)))) 1.0))

;; Exercise 1.36
(define (fixed-point-println f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

(define (answer-to-1-36-damp)
  (fixed-point-println (lambda (x)
			 (average x (/ (log 1000)
				       (log x))))
		       1.1))
(define (answer-to-1-36-no-damp)
  (fixed-point-println (lambda (x)
			 (/ (log 1000)
			    (log x)))
		       1.1))
;; Exercise 1.37

;; (All this below assumes that we start the series with one (as in the book)

;; iterative solution - 'bottom up'
(define (cont-frac n d k)
  (define (iter result term)
    (if (= 0 term)
	result
	(iter (/ (n term)
		 (+ (d term) result))
	      (dec term))))
  (iter 0 k))

;; recursive solution - 'top down'
(define (cont-frac-r n d k)
  (define (recursion term)
    (/ (n term)
       (+ (d term)
	  (if (= term k) 0 (recursion (inc term))))))
  (recursion 1))

(define (reciprocal-of-phi approx) (cont-frac-r (lambda (i) 1.0)
				       (lambda (i) 1.0)
				       approx))

;; Exercise 1.38
(define (e-approx approx)
  (+ 2 (cont-frac (lambda (i) 1.0)
		  (lambda (i) (if (= 2 (modulo i 3))
				  (* 2 (ceiling (/ i 3)))
				  1))
		  approx)))

;; Exercise 1.39
(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= 1 i) x (- (square x))))
	     (lambda (i) (- (* 2.0 i) 1))
	     k))

;; 1.3.4 Procedures as Returned Values

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt-fp x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
	       1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
	       1.0))

(define (deriv g)
  (let ((dx 0.000001))
    (lambda (x)
      (/ (- (g (+ x dx)) (g x))
	 dx))))

(define (newton-transform g)
  (lambda (x)
    (- x
       (/ (g x)
	  ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt-newton x)
  (newtons-method (lambda (y) (- (square y) x))
		  1.0))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt-damp-transform x)
  (fixed-point-of-transform (lambda (y) (/ x y))
			    average-damp
			    1.0))

(define (sqrt-newton-transform x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
			    newton-transform
			    1.0))

;; Exercise 1.40
(define (cubic a b c)
  (lambda (x) (+ (cube x)
		 (* a (square x))
		 (* b x)
		 c)))

;; should be (next to) nothing ((cubic 1 2 3) (newtons-method (cubic 1 2 3) 1))

;; Exercise 1.41
(define (double f)
  (lambda (x) (f (f x))))

;; (((double (double double)) inc) 5)
;; (((lambda (x) ((double double) ((double double) x))) inc) 5)
;; (((double double) ((double double) inc)) 5)
;; (((lambda (x) (double (double x))) ((double double) inc)) 5)
;; (((lambda (x) (double (double x))) ((lambda (x) (double (double x))) inc)) 5)
;; (((lambda (x) (double (double x))) (double (double inc))) 5)
;; (((lambda (x) (double (double x))) (lambda (x) ((double inc) ((double inc) x)))) 5)
;; (((double (double (lambda (x) ((double inc) ((double inc) x)))))) 5)
;; (((double (double (lambda (x) ((lambda (y) (inc (inc y))) ((double inc) x)))))) 5)
;; (((double (double (lambda (x) ((lambda (y) (inc (inc y))) ((lambda (y) (inc (inc y))) x)))))) 5)
;; (((double (double (lambda (x) (inc (inc (inc (inc x)))))))) 5)
;; (((double ((lambda (y) ((lambda (x) (inc (inc (inc (inc x)))))
;; 			((lambda (x) (inc (inc (inc (inc x))))) y)))))) 5)
;; (((double ((lambda (y) ((lambda (x) (inc (inc (inc (inc (inc (inc (inc (inc x))))))))) y))))) 5)
;; (((double (lambda (x) (inc (inc (inc (inc (inc (inc (inc (inc x))))))))))) 5)
;; ((((lambda (y)
;;      ((lambda (x) (inc (inc (inc (inc (inc (inc (inc (inc x)))))))))
;;       ((lambda (x) (inc (inc (inc (inc (inc (inc (inc (inc x))))))))) y))))) 5)
;; ((((lambda (y)
;;      ((lambda (x) (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc x)))))))))))))))))) y)))) 5)
;; ((lambda (x) (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc x)))))))))))))))))) 5)
;; (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc 5)))))))))))))))))
;; 21
;;
;; (16 applications of `inc` to 5)
;; (The double application (2) of the double application (4) of the double application (16) of `inc' to 5)
;; compare (expt 2 (expt 2 2))
;; consider (expt 2 (expt 2 (expt 2))) ~= (((double (double (double double))) inc) 0)

;; Exercise 1.42
(define (compose f g)
  (lambda (x) (f (g x))))

;; Exercise 1.43
(define (repeated f n)
  (define (iter result count)
    (if (= count n)
	result
	(iter (f result) (inc count))))
  (lambda (x) (iter x 0)))
