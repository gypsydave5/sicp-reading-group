#lang sicp
(#%provide (all-defined))

(define (println n)
  (display n)
  (newline))

(define (average a b)
  (/ (+ a b) 2))
