#lang sicp
(#%require "helpers.rkt")

;;* 2.3.4 Huffman Encoding Trees

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

;; Decoding
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

;; Sets of weighted elements
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

;;* Exercise 2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))
(define sample-message
  '(0 1 1 0 0 1 0 1 0 1 1 1 0))
;;** Answer:
(decode sample-message sample-tree)
;; =>
;; {A D A B B C A}

(define (has-symbol? symbol seq)
  (any? (lambda (x) (equal? x symbol)) seq))
;; => #<procedure:any?>

;;* Exercise 2.68
(define (encode-symbol symbol tree)
  (cond ((leaf? tree) '())
        ((has-symbol? symbol (symbols (left-branch tree)))
         (cons 0 (encode-symbol symbol (left-branch tree))))
        ((has-symbol? symbol (symbols (right-branch tree)))
         (cons 1 (encode-symbol symbol (right-branch tree))))
        (else (error "symbol not in tree -- ENCODE SYMBOL" symbol))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))
;; => #<procedure:any?>

;;** Answer:
(encode '(A D A B B C A) sample-tree)
;; =>
;; {0 1 1 0 0 1 0 1 0 1 1 1 0}

;;* Exercise 2.69
(define pairs '((A 4) (B 2) (C 1) (D 1)))

(define (successive-merge set)
  (if (= 1 (length set))
      (car set)
      (successive-merge (adjoin-set (make-code-tree (car set)
                                                    (cadr set))
                                    (cddr set)))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))
;;** Answer:
(generate-huffman-tree pairs)
;; =>
;; {{leaf A 4}
;;  {{leaf B 2}
;;   {{leaf D 1} {leaf C 1} {D C} 2}
;;   {B D C}
;;   4}
;;  {A B D C}
;;  8}
;;** Compare:
sample-tree
;; =>
;; {{leaf A 4}
;;  {{leaf B 2}
;;   {{leaf D 1} {leaf C 1} {D C} 2}
;;   {B D C}
;;   4}
;;  {A B D C}
;;  8}

(define another-set-of-pairs
  '((A 8) (B 3) (C 1) (D 1) (E 1) (F 1) (G 1) (H 1)))
;;** Just checking...:
(generate-huffman-tree another-set-of-pairs)
;; =>
;; {{leaf A 8}
;;  {{{{leaf H 1} {leaf G 1} {H G} 2}
;;    {{leaf F 1} {leaf E 1} {F E} 2}
;;    {H G F E}
;;    4}
;;   {{{leaf D 1} {leaf C 1} {D C} 2}
;;    {leaf B 3}
;;    {D C B}
;;    5}
;;   {H G F E D C B}
;;   9}
;;  {A H G F E D C B}
;;  17}

;;* Exercise 2.70
;; see https://en.wikipedia.org/wiki/Get_a_Job_(song)
(define get-a-job-pairs
  '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3) (YIP 9) (WAH 1)))
(define get-a-job-lyrics
  '(GET A JOB
        SHA NA NA NA NA NA NA NA NA
        GET A JOB
        SHA NA NA NA NA NA NA NA NA
        WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
        SHA BOOM))

;;** Answer:
(encode get-a-job-lyrics (generate-huffman-tree get-a-job-pairs))
;; =>
;;  {1 1 1 1 0 1 1 1 0 1 1 1 1 1 0 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 0 1 1 1 0 1 1 1 1 1 0 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 0 1 1 1 1 1 1 0}
;;** Bits for encoding:
(length (encode get-a-job-lyrics (generate-huffman-tree get-a-job-pairs)))
;; => 87
;;** Fixed-length encoding length:
(* 4 (length get-a-job-lyrics))
;; => 144

;;* Exercise 2.71
;;** n = 5:
(map (lambda (x) (expt 2 x)) (range 0 5))
;; =>
;; {1 2 4 8 16}
;;** 5 tree:
(generate-huffman-tree (zip '(a b c d e) (map (lambda (x) (expt 2 x)) (range 0 5))))
;; =>
;; {{{{{leaf a 1} {leaf b 2} {a b} 3}
;;    {leaf c 4}
;;    {a b c}
;;    7}
;;   {leaf d 8}
;;   {a b c d}
;;   15}
;;  {leaf e 16}
;;  {a b c d e}
;;  31}
;; here the issue is that the pair of the lowest two is _always_ too small -
;; so the depth is _maximized_ - and depth == require bits for least frequent - 5 or 10
;;** n = 10:
(map (lambda (x) (expt 2 x)) (range 0 10))
;; =>
;; {1 2 4 8 16 32 64 128 256 512}

;; Exercise 2.72

