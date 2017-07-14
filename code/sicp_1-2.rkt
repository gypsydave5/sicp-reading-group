#lang sicp
(#%require (rename racket racket-random random))
(define (random n)
  (if (and (exact? n) (integer? n))
      (racket-random n)
      (* n (racket-random))))
					; 1.2 Procedures and the Processes They Generate

;; 1.2.1 - Linear Recursion and Iteration

;;;; linear recursive factorial
(define (factorial-lr n)
  (if (= n 1)
      1
      (* n (factorial-lr (- n 1))))) ;; note - not recursive call not in the tail position

;;;; linear iterative factorial - with lexical scoping of the `inner` function
(define (factorial-li n)
  (define (iter product counter max-count)
    (if (> counter max-count)
	product
	(iter (* counter product) (+ counter 1) max-count)))
  (iter 1 1 n))

;; Exercise 1.9
(define (+lr a b)
  (if (= a 0)
      b
      (inc (+lr (dec a) b)))) ;; `inc` in the tail position

;; (+lr 4 5)
;; (inc (+ (dec 4) 5))
;; (inc (+ 3 5))
;; (inc (inc (+ (dec 3) 5)))
;; (inc (inc (+ 2 5)))
;; (inc (inc (inc (+ (dec 2) 5))))
;; (inc (inc (inc (+ 1 5))))
;; (inc (inc (inc (inc (+ (dec 1) 5))))) <== recursive max depth
;; (inc (inc (inc (inc (+ 0 5)))))
;; (inc (inc (inc (inc 5))))
;; (inc (inc (inc 6)))
;; (inc (inc 7)
;; (inc 8)
;; 9

(define (+li a b)
  (if (= a 0)
      b
      (+li (dec a) (inc b)))) ;; `+li` in the tail position

;; (+li 4 5)
;; (+li (dec 4) (inc 5))
;; (+li 3 6)
;; (+li (dec 3) (inc 6))
;; (+li 2 7)
;; (+li (dec 2) (inc 7)) <== linear - max length constant
;; (+li 1 8)
;; (+li (dec 1) (inc 8)
;; (+li 0 9)
;; 9

;; Exercise 1.10 - Ackermann's function.
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (dec x)
                 (A x (dec y))))))

;; (A 1 10)
;; (A (dec 1) (A 1 (dec 10)))
;; (A 0 (A 1 9))
;; (A 0 (A 0 (A 1 8)))
;; (A 0 (A 0 (A 0 (A 1 7))))
;; (A 0 (A 0 (A 0 (A 0 (A 1 6)))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4)))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2)))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2)))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8)))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 16))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 32)))))
;; (A 0 (A 0 (A 0 (A 0 64))))
;; (A 0 (A 0 (A 0 128)))
;; (A 0 (A 0 256))
;; (A 0 512)
;; 1024

;; (A 2 4)
;; (A 1 (A 2 3))
;; (A 1 (A 1 (A 2 2)))
;; (A 1 (A 1 (A 1 (A 2 1))))
;; (A 1 (A 1 (A 1 2)))
;; (A 1 (A 1 (A 0 (A 1 1))))
;; (A 1 (A 1 (A 0 2)))
;; (A 1 (A 1 4))
;; (A 1 (A 0 (A 1 3)))
;; (A 1 (A 0 (A 0 (A 1 2))))
;; (A 1 (A 0 (A 0 (A 0 (A 1 1)))))
;; (A 1 (A 0 (A 0 (A 0 2))))
;; (A 1 (A 0 (A 0 4)))
;; (A 1 (A 0 8))
;; (A 1 16)
;; (A 0 (A 1 15))
;; (A 0 (A 0 (A 1 14)))
;; (A 0 (A 0 (A 0 (A 1 13))))
;; (A 0 (A 0 (A 0 (A 0 (A 1 12)))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 11))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 10)))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 1024)))))) ;; by substitution
;; (A 0 (A 0 (A 0 (A 0 (A 0 2048)))))
;; (A 0 (A 0 (A 0 (A 0 4096))))
;; (A 0 (A 0 (A 0 8192)))
;; (A 0 (A 0 16384))
;; (A 0 32768)
;; 65536

;; (A 3 3)
;; (A 2 (A 3 2))
;; (A 2 (A 2 (A 3 1)))
;; (A 2 (A 2 2))
;; (A 2 (A 1 (A 2 1)))
;; (A 2 (A 1 2))
;; (A 2 (A 0 (A 1 1)))
;; (A 2 (A 0 2))
;; (A 2 4)
;; (A 1 (A 1 4))
;; (A 1 (A 0 (A 1 3)))
;; (A 1 (A 0 (A 0 (A 1 2))))
;; (A 1 (A 0 (A 0 (A 0 (A 1 1)))))
;; (A 1 (A 0 (A 0 (A 0 2))))
;; (A 1 (A 0 (A 0 4)))
;; (A 1 (A 0 8))
;; (A 1 16)
;;
;; and by substitution
;; 65536

(define (f n) (A 0 n)) 			;; 2n
(define (g n) (A 1 n)) 			;; 2^{n}
(define (h n) (A 2 n)) 			;; 2^2^...(n-1) (tetration)

;; 1.2.2 Tree recursion

(define (fib-tree n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(else (+ (fib-tree (- n 1))
		 (fib-tree (- n 2))))))

(define (fib-i n)
  (define (iter a b count)
    (if (= count 0)
	b
	(iter b (+ a b) (- count 1))))
  (iter 1 0 n))

(define (count-change amount)
  (define (first-denomination kinds-of-coins)
    (cond ((= kinds-of-coins 1) 1)
	  ((= kinds-of-coins 2) 2)
	  ((= kinds-of-coins 3) 5)
	  ((= kinds-of-coins 4) 10)
	  ((= kinds-of-coins 5) 20)
	  ((= kinds-of-coins 6) 50)))
  (define (cc amount kinds-of-coins)
    (cond ((= amount 0) 1)
	  ((or (< amount 0) (= kinds-of-coins 0)) 0)
	  (else (+ (cc amount
		       (- kinds-of-coins 1))
		   (cc (- amount (first-denomination kinds-of-coins))
		       kinds-of-coins)))))
  (cc amount 6))

(define (count-change-i amount)
  (define (cc-50s amount acc)
    (cond ((= amount 0) (+ 1 acc))
	  ((> 0 amount) acc)
	  (else (cc-50s (- amount 50) (cc-20s amount acc)))))
  (define (cc-20s amount acc)
    (cond ((= amount 0) (+ 1 acc))
	  ((> 0 amount) acc)
	  (else (cc-20s (- amount 20) (cc-10s amount acc)))))
  (define (cc-10s amount acc)
    (cond ((= amount 0) (+ 1 acc))
	  ((> 0 amount) acc)
	  (else (cc-10s (- amount 10) (cc-5s amount acc)))))
  (define (cc-5s amount acc)
    (cond ((= amount 0) (+ 1 acc))
	  ((> 0 amount) acc)
	  (else (cc-5s (- amount 5) (cc-2s amount acc)))))
  (define (cc-2s amount acc)
    (cond ((= amount 0) (+ 1 acc))
	  ((> 0 amount) acc)
	  (else (cc-2s (- amount 2) (cc-1s amount acc)))))
  (define (cc-1s amount acc)
    (cond ((= amount 0) (+ 1 acc))
	  ((> 0 amount) acc)
	  (else (cc-1s (- amount 1) (cc-0s amount acc)))))
  (define (cc-0s amount acc)
    acc)
  (cc-50s amount 0))

;; (count-change-i 10)
;; (cc-50s 10 0)
;; (cc-50s (- 10 50) (cc-20s 10 0))
;; (cc-50s -40 (cc-20s (- 10 20) (cc-10s 10 0)))
;; (cc-20s -10 (cc-10s (- 10 10) (cc-5s 10 0)))
;; (cc-10s 0 (cc-5s 10 0))
;; (+ 1 (cc-5s (- 10 5) (cc-2s 10 0)))
;; (+ 1 (cc-5s 5 (cc-2s (- 10 2) (cc-1s 10 0))))
;; (+ 1 (cc-5s (- 5 5) (cc-2s 5 (cc-2s (- 10 2) (cc-1s 10 0)))))
;; (+ 1 (cc-5s 0 (cc-2s 5 (cc-2s 8 (cc-1s 10 0)))))
;; (+ 1 (+ 1 (cc-2s (- 5 2) (cc-1s 5 (cc-2s 8 (cc-1s 10 0))))))
;; (+ 1 (+ 1 (cc-2s 3 (cc-1s (- 5 1) (cc-0s (cc-2s 8 (cc-1s 10 0)))))))
;; ...
;; I think that this is not ultimately iterative

;; Exercise 1.11
(define (ff n)
  (if (< n 3)
      n
      (+ (f (- n 1))
	 (* 2 (f (- n 2)))
	 (* 3 (f (- n 3))))))


;; Exercise 1.12
;; Exercise 1.13

;; 1.2.4 Exponentiation

(define (square n)
  (* n n))

(define (expt-r b n)
  (if (= n 0)
      1
      (* b (expt-r b (- n 1)))))

(define (expt-l b n)
  (define (iter b counter product)
    (if (= counter 0)
	product
	(iter b
	      (- counter 1)
	      (* b product))))
  (iter b n 1))

(define (fast-expt-r b n)
  (cond ((= n 0) 1)
	((even? n) (square (fast-expt-r b (/ n 2))))
	(else (* b (fast-expt-r b (- n 1))))))
;; Exercise 1.16
;; (= (expt (square b) (/ n 2)) (square (expt b (/ n2))))
;; substitution in the even branch
;; so only need to keep track of the (* b ...) in the else
;; which is handled by the value of `a'

(define (fast-expt-l b n)
  (define (iter b n a)
    (cond ((= n 0) a)
	  ((even? n) (iter (square b) (/ n 2) a))
	  (else (iter b (- n 1) (* b a)))))
  (iter b n 1))

;; Exercise 1.17

(define (*r a b)
  (if (= b 0)
      0
      (+ a (*r a (- b 1)))))
(define (double x)
  (+ x x))

(define (halve x)
  (/ x 2))

(define (*rf b n)
  (cond ((= n 0) 0)
	((even? n) (double (*rf b (/ n 2))))
	(else (+ b (*rf b (- n 1))))))

;; Exercise 1.18
(define (*lf a b)
  (define (iter a b acc)
    (cond ((= b 0) acc)
	  ((even? b) (iter (double a) (halve b) acc))
	  (else (iter a (dec b) (+ acc a)))))
  (iter a b 0))

;; Exercise 1.19
;; a' = bq + aq + ap
;; b' = bp + aq
;; b'' = b'p + a'q
;;     = (bp + aq)p + (bq + aq + ap)q
;;     = bpp + aqp + bqq + aqq + apq
;;     = (2aqp + aqq) + (bpp + bqq)
;;     = (2qp + q^2)a + (p^2 + q^2)b
;; b'p + a'q = (2qp + q^2)a + (p^2 + q^2)b
;; p' = (p^2 + q^2) ;; by substitution
;; q' = (2qp + q^2)

(define (best-fib n)
  (best-fib-iter 1 0 0 1 n))

(define (best-fib-iter a b p q count)
  (cond ((= count 0) b)
	((even? count)
	 (best-fib-iter a
			b
			(+ (square p) (square q))
			(+ (* 2 p q) (square q))
			(/ count 2)))
	(else (best-fib-iter (+ (* b q) (* a q) (* a p))
			     (+ (* b p) (* a q))
			     p
			     q
			     (- count 1)))))

;; 1.2.5 Greatest Common Divisors

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; Exercise 1.20

;; 1.2.6

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (prime?? n)
  (= n (smallest-divisor n)))


(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder (square (expmod base (/ exp 2) m))
		    m))
	(else
	 (remainder (* base (expmod base (- exp 1) m))
		    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
	((fermat-test n) (fast-prime? n (- times 1)))
	(else false)))

;; Exercise 1.22
(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime?? n)
      (report-prime n (- (runtime) start-time))
      false))

(define (report-prime n elapsed-time)
  (display "*** ")
  (display n)
  (display " time: ")
  (display elapsed-time)
  (display " ***")
  (newline))

(define (search-for-primes lower count)
  (cond ((= 0 count) 0)
	(else (if (timed-prime-test lower)
		  (search-for-primes (+ 2 lower) (- count 1))
		  (search-for-primes (+ 2 lower) count)))))
