#lang sicp

;; 2.3.1 Quotation

;; "Lists containing symbols can look just like the expressions of our language"

;; "Say your name" vs. "Say 'your name'"

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(memq 'apple '(pear banana prune))            ;;  #f
(memq 'apple '(x (apple sauce) y apple pear)) ;;  {apple pear}

;; Exercise 2.53
;; (a b c)
(list 'a 'b 'c)                         ;;  {a b c}
;; ((george))
(list (list 'george))                   ;;  {{george}}
;; ((y1 y2))
(cdr '((x1 x2) (y1 y2)))                ;;  {{y1 y2}}
;; (y1 y2)
(cadr '((x1 x2) (y1 y2)))               ;;  {y1 y2}
;; #f
(pair? (car '(a short list)))           ;;  #f
;; #f
(memq 'red '((red shoes) (blue socks))) ;;  #f
;; (red shoes blue socks)
(memq 'red '(red shoes blue socks))     ;;  {red shoes blue socks}

;; Exercise 2.54
(define (equal? l1 l2)
  (cond ((and (pair? l1) (pair? l2))
         (and (equal? (car l1) (car l2))
              (equal? (cdr l1) (cdr l2))))
        (else (eq? l1 l2))))

;; Exercise 2.55
;; ''abracadabra = (quote (quote abracadabra)) = '(quote abracadabra)
;;               = (list 'quote 'abracadabra)
;;               = (quote abracadabra)
;; ... (car ''abracadabra) = quote
