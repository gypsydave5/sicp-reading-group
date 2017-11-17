#lang sicp
(#%provide (all-defined))

(define (println n)
  (display n)
  (newline))

(define (average a b)
  (/ (+ a b) 2))

(define (any? pred seq)
  (cond ((null? seq) false)
        ((pred (car seq)) true)
        (else (any? pred (cdr seq)))))

(define (range min max)
  (if (= min max)
      '()
      (cons min (range (+ 1 min) max))))

(define (zip seq1 seq2)
  (if (or (null? seq1) (null? seq2))
      '()
      (cons (list (car seq1)
                  (car seq2))
            (zip (cdr seq1) (cdr seq2)))))
