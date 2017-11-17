#lang sicp

;; 2.3.2 Example: Symbolic Differentiation

(define (variable? x)
  (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (make-sum a1 a2)
  (list '+ a1 a2))

(define (make-product m1 m2)
  (list '* m1 m2))

(define (sum? x)
  (and (pair? x)
       (eq? (car x) '+)))

(define (addend s)
  (cadr s))

(define (augend s)
  (caddr s))

(define (product? x)
  (and (pair? x)
       (eq? (car x) '*)))

(define (multiplier p)
  (cadr p))

(define (multiplicand p)
  (caddr p))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(deriv '(+ x 3) 'x)                     ;;  {+ 1 0}
(deriv '(* x y) 'x)                     ;;  {+ {* x 0} {* 1 y}}
(deriv '(* (* x y) (+ x 3)) 'x)
;; {+ {* {* x y}
;;       {+ 1 0}}
;;    {* {+ {* x 0}
;;          {* 1 y}}
;;       {+ x 3}}}

;; simplification

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum-s a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product-s m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (deriv-s exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum-s (deriv-s (addend exp) var)
                     (deriv-s (augend exp) var)))
        ((product? exp)
         (make-sum-s
          (make-product-s (multiplier exp)
                          (deriv-s (multiplicand exp) var))
          (make-product-s (deriv-s (multiplier exp) var)
                          (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))


(deriv-s '(+ x 3) 'x)                   ;;  1
(deriv-s '(* x y) 'x)                   ;;  y
(deriv-s '(* (* x y) (+ x 3)) 'x)       ;;  {+ {* x y} {* y {+ x 3}}}

;; Exercise 2.56

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((and (number? exponent) (number? base)) (expt base exponent))
        (else (list '** base exponent))))

(define (exponentiation? exp)
  (and (pair? exp)
       (eq? (car exp) '**)))

(define (base exp)
  (cadr exp))

(define (exponent exp)
  (caddr exp))

;; TODO: will leave this until I understand calculus ...
;; OK - now I know what to do (but I don't understand calculus)
;;
;; d(uⁿ)/dy = nuⁿ⁻¹(du/dx)
;;
;; I wonder why...
;;

(define (deriv-e exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum-s (deriv-e (addend exp) var)
                     (deriv-e (augend exp) var)))
        ((product? exp)
         (make-sum-s
          (make-product-s (multiplier exp)
                          (deriv-e (multiplicand exp) var))
          (make-product-s (deriv-e (multiplier exp) var)
                          (multiplicand exp))))
        ((exponentiation? exp)
         (make-product-s
          (make-product-s (exponent exp)
                          (make-exponentiation (base exp)
                                               (make-sum-s (exponent exp) -1)))
          (deriv-e (base exp) var)))
        (else
         (error "unknown expression type -- DERIV-E" exp))))

(deriv-e '(** x 4) 'x)                  ;;  {* 4 {** x 3}}
(deriv-e '(* 4 (** x 3)) 'x)            ;;  {* 4 {* 3 {** x 2}}}
(deriv-e '(** x (+ x 1)) 'x)            ;;  {* {+ x 1} {** x {+ {+ x 1} -1}}}
;; ax² + bx + c
(deriv-e '(+ (* a (** x 2)) (+ (* b x) c)) 'x) ;;  {+ {* a {* 2 x}} b}
(deriv-e '(** x 2.5) 'x)
(deriv-e '(** x y) 'x)

;; Exercise 2.57
(define (make-sum-a a1 . a2)
  (cond ((null? a2) a1)
        ((=number? a1 0) (apply make-sum-a a2))
        ((null? (cdr a2)) (make-sum-s a1 (car a2)))
        ((and (number? a1) (number? (car a2)))
         (apply make-sum-a (cons (+ a1 (car a2)) (cdr a2))))
        ((number? a1) (list '+ (car a2) (apply make-sum-a (cons a1 (cdr a2)))))
        (else (list '+ a1 (apply make-sum-a a2)))))

(define (addend-a exp)
  (cadr exp))

(define (augend-a exp)
  (apply make-sum-a (cddr exp)))

(define (make-product-a a1 . a2)
  (cond ((null? a2) a1)
        ((=number? a1 0) 0)
        ((null? (cdr a2)) (make-product-s a1 (car a2)))
        ((and (number? a1) (number? (car a2)))
         (apply make-product-a (cons (* a1 (car a2)) (cdr a2))))
        ((number? a1) (list '* (car a2) (apply make-sum-a (cons a1 (cdr a2)))))
        (else (list '* a1 (apply make-product-a a2)))))

(define (multiplier-a exp)
  (cadr exp))

(define (multiplicand-a exp)
  (apply make-product-a (cddr exp)))

(define (deriv-a exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum-a (deriv-a (addend-a exp) var)
                     (deriv-a (augend-a exp) var)))
        ((product? exp)
         (make-sum-a
          (make-product-a (multiplier-a exp)
                          (deriv-a (multiplicand-a exp) var))
          (make-product-a (deriv-a (multiplier-a exp) var)
                          (multiplicand-a exp))))
        ((exponentiation? exp)
         (make-product-a
          (make-product-a (exponent exp)
                          (make-exponentiation (base exp)
                                               (make-sum-a (exponent exp) -1)))
          (deriv-a (base exp) var)))
        (else
         (error "unknown expression type -- DERIV-A" exp))))

(deriv-a '(+ (* a (** x 2)) (* b x) c) 'x) ;;  {+ {* a {* 2 x}} b}
(deriv-a '(** x (** x (** x x))) 'x)    ;;  {* {** x {** x x}} {** x {+ {** x {** x x}} -1}}}

;; Exercise 2.58

(define (sum-i? exp)
  (and (pair? exp)
       (pair? (cdr exp))
       (eq? '+ (cadr exp))))

(define (make-sum-i a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (addend-i exp)
  (car exp))

(define (augend-i exp)
  (caddr exp))

(define (product-i? exp)
  (and (pair? exp)
       (pair? (cdr exp))
       (eq? '* (cadr exp))))

(define (make-product-i m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (multiplicand-i exp)
  (car exp))

(define (multiplier-i exp)
  (caddr exp))

(define (exponentiation-i? exp)
  (and (pair? exp)
       (pair? (cdr exp))
       (eq? '** (cadr exp))))

(define (make-exponentiation-i b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        ((and (number? b) (number? e)) (expt b e))
        (else (list b '** e))))

(define (exponent-i exp)
  (caddr exp))

(define (base-i exp)
  (car exp))

(define (deriv-i exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum-i? exp)
         (make-sum-i (deriv-i (addend-i exp) var)
                     (deriv-i (augend-i exp) var)))
        ((product-i? exp)
         (make-sum-i
          (make-product-i (multiplier-i exp)
                          (deriv-i (multiplicand-i exp) var))
          (make-product-i (deriv-i (multiplier-i exp) var)
                          (multiplicand-i exp))))
        ((exponentiation-i? exp)
         (make-product-i
          (make-product-i (exponent-i exp)
                          (make-exponentiation-i (base-i exp)
                                                 (make-sum-i (exponent-i exp) -1)))
          (deriv-i (base-i exp) var)))
        (else
         (error "unknown expression type -- DERIV-i" exp))))

(deriv-i '((a * (x ** 2)) + ((b * x) + c)) 'x) ;;  {{{2 * x} * a} + b}

;; b - hard mode

(define (any? predicate? list)
  (cond ((null? list) false)
        ((predicate? (car list)) true)
        (else (any? predicate? (cdr list)))))

(define (none? predicate? list)
  (cond ((null? list) true)
        ((predicate? (car list)) false)
        (else (none? predicate? (cdr list)))))

(define (before predicate? ls)
  (define (iter before predicate? after)
    (cond ((null? after) before)
          ((predicate? (car after)) (reverse before))
          (else (iter (cons (car after) before) predicate? (cdr after)))))
  (iter (list) predicate? ls))

(define (after p? ls)
  (cond ((null? ls) ls)
        ((p? (car ls)) (cdr ls))
        (else (after p? (cdr ls)))))

(define (remove-parens exp)
  (cond ((not (pair? exp)) exp)
        ((< 1 (length exp)) exp)
        (else (remove-parens (car exp)))))

(define (sum-ii? exp)
  (and (any? (lambda (x) (eq? x '+)) exp)))

(define make-sum-ii make-sum-i)

(define (addend-ii exp)
  (remove-parens (before (lambda (x) (eq? x '+)) exp)))

(define (augend-ii exp)
  (remove-parens (after (lambda (x) (eq? x '+)) exp)))

(define (product-ii? exp)
  (and (any? (lambda (x) (eq? x '*)) exp)
       (none? (lambda (x) (eq? x '+)) exp)))

(define make-product-ii make-product-i)

(define (multiplier-ii exp)
  (remove-parens (after (lambda (x) (eq? x '*)) exp)))

(define (multiplicand-ii exp)
  (remove-parens (before (lambda (x) (eq? x '*)) exp)))

(define (exponentiation-ii? exp)
  (and (any? (lambda (x) (eq? x '**)) exp)
       (none? (lambda (x) (and (eq? x '*) (eq? x '+))) exp)))

(define (make-exponentiation-ii base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((and (number? base) (number? exponent)) (expt base exponent))
        (else (list base '** exponent))))

(define (exponent-ii exp)
  (remove-parens (after (lambda (x) (eq? x  '**)) exp)))

(define (base-ii exp)
  (remove-parens (before (lambda (x) (eq? x '**)) exp)))

(define (deriv-ii exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum-ii? exp)
         (make-sum-ii (deriv-ii (addend-ii exp) var)
                      (deriv-ii (augend-ii exp) var)))
        ((product-ii? exp)
         (make-sum-ii
          (make-product-ii (multiplier-ii exp)
                           (deriv-ii (multiplicand-ii exp) var))
          (make-product-ii (deriv-ii (multiplier-ii exp) var)
                           (multiplicand-ii exp))))
        ((exponentiation-ii? exp)
         (make-product-ii
          (make-product-ii (exponent-ii exp)
                           (make-exponentiation-ii (base-ii exp)
                                                   (make-sum-ii (exponent-ii exp) -1)))
          (deriv-ii (base-ii exp) var)))
        (else
         (error "unknown expression type -- DERIV-II" exp))))

(deriv-ii '(a * x ** 2 + b * x + c) 'x) ;;  {{{2 * x} * a} + b}

;; This ... this has been fun. Would've enjoyed continuing with a function to simplify a general formula.
