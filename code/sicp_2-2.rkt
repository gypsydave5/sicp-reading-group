#lang sicp
(#%require "helpers.rkt")
(#%require "sicp_1-2.rkt")

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25))

(list-ref squares 3)                    ;;  16

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(length squares)                        ;;  5

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

;; Exercise 2.17
(define (last-pair l)
  (if (null? (cdr l))
      l
      (last-pair (cdr l))))
(last-pair squares)                     ;;  {25}

;; Exercise 2.18
(define (reverse l)
  (define (iter r l)
    (if (null? l)
        r
        (iter (cons (car l) r) (cdr l))))
  (iter nil l))
(reverse squares)                       ;;  {25 16 9 4 1}

;; Exercise 2.19

(define no-more? null?)
(define except-first-denomination cdr)
(define first-denomination car)

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1))


(cc 100 us-coins)                       ;;  292
(cc 100 uk-coins)                       ;;  4563
(cc 100 (reverse uk-coins))             ;;  4563 (apparently not)

;; Exercise 2.20

(define (same-parity i . is)
  (define (iter res is)
    (cond ((null? is)
           (reverse res))
          ((equal? (even? i) (even? (car is)))
           (iter (cons (car is) res) (cdr is)))
          (else (iter res (cdr is)))))
  (iter (list i) is))
(same-parity 1 2 3 4 5 6 7)             ;;  {1 3 5 7}
(same-parity 2 3 4 5 6 7)               ;;  {2 4 6}

;;

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))

(scale-list (list 1 2 3 4 5) 10)        ;;  {10 20 30 40 50}

;; Exercise 2.21
(define (square-list items)
  (if (null? items)
      nil
      (cons (* (car list) (car list)) (cdr list))))

(define (square-list-m items)
  (map (lambda (x) (* x x)) list))

;; Exercise 2.22
;; When processed iteratively, a list is reconstructed front-to-back - the first element will be
;; be `cons`ed onto the list you're building - and so will become last.
;;
;; The adjusted version will error as now the first iteration will form
;; `(nil . (square (car things)))`, no longer providing a terminating condition for the list
;; - no longer a list!

;; Exercise 2.23
(define (for-each f items)
  (cond ((null? items) #t)
        (else
         (f (car items))
         (for-each f (cdr items)))))

(define (m-for-each f items)
  (map display items)
  #t)

(for-each display (list 1 2 3 4))
(m-for-each display (list 1 2 3 4))

;; 2.2.2 Hierarchical Structures

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x) 1))
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

;; Exercise 2.24
;; (1 (2 (3 4)))
;; 1----2----3
;;       \---4

;; Exercise 2.25
;; cadaddr - (car (cdr (car (cdr (cdr '(1 3 (5 7) 9))))))
;; caar
;; caddddddr

;; Exercise 2.26
;; (append  x y) ;=> '(1 2 3 4 5 6)
;; (cons x y)    ;=> '((1 2 3) 4 5 6)
;; (list x y)    ;=> '((1 2 3) (4 5 6))

;; Exercise 2.27
(define (deep-reverse x)
  (cond ((not (pair? x)) x)
        ((null? (cdr x)) (deep-reverse (car x)))
        (else (cons (deep-reverse (cdr x))
                    (deep-reverse (car x))))))

(define (deep-reverse-i list)
  (define (iter result x)
    (cond ((null? x) result)
          ((not (pair? (car x)))
           (iter (cons (car x) result) (cdr x)))
          (else
           (iter (cons (deep-reverse (car x)) result) (cdr x)))))
  (iter '() list))

;; Exercise 2.28
(define (fringe-r tree)
  (cond ((null? tree)
         tree)
        ((not (list? (car tree)))
         (append (list (car tree)) (fringe-r (cdr tree))))
        (else
         (append (fringe-r (car tree)) (fringe-r (cdr tree))))))

;; Exercise 2.29
(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))

;; a
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cadr mobile))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cadr branch))

(define (mobile-weight? mobile)
  (integer? mobile))

;; b
(define (total-weight mobile)
  (if (mobile-weight? mobile)
      mobile
      (+ (total-weight (branch-structure (left-branch mobile)))
         (total-weight (branch-structure (right-branch mobile))))))

(define mobile
  (make-mobile (make-branch 2 5)
               (make-branch 5 2)))

(define unbalanced-mobile
  (make-mobile (make-branch 2 5)
               (make-branch 5 5)))

;; c
(define (torque branch)
  (* (branch-length branch) (total-weight (branch-structure branch))))

(define (balanced? mobile)
  (cond ((mobile-weight? mobile) #t)
        ((= (torque (left-branch mobile)) (torque (right-branch mobile)))
         (and (balanced? (branch-structure (left-branch mobile)))
              (balanced? (branch-structure (right-branch mobile)))))
        (else #f)))

;; d
;; Very little I think :D should just be the selectors.

;; Mapping over trees
(define (scale-tree tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

(define (scale-tree-m tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       map))

;; Exercise 2.30
(define tree30 (list 1
                     (list 2 (list 3 4) 5)
                     (list 6 7)))

(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(define (square-tree-m tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (* sub-tree sub-tree)))
       tree))

;; Exercise 2.31
(define (tree-map f tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map f sub-tree)
             (f sub-tree)))
       tree))

(define (square number) (* number number))

;; Exercise 2.32
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest
                (map (lambda (x) (cons (car s) x)) rest)))))

;; every element gets `cons`d to the every subset of the rest of the elements
;; and then added to that set (to form the subset of the rest + the current element
;; The base case is a list with an empty set in
;; the empty list. Next, the elment `3` is consed
;; with the empty set, and added appended to that list - ((), (3)) -
;; introducing each variable and adding it to each previous combination.

;; 2.2.3 Sequences as Conventional Interfaces

(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))

(define (fib n) n)

(define (even-fibs n)
  (define (next k)
    (if (> k n)
        nil
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(enumerate-interval 1 10)               ;;  {1 2 3 4 5 6 7 8 9 10}

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define (sum-odd-squares-flow tree)
  (accumulate +
              0
              (map square
                   (filter odd?
                           (enumerate-tree tree)))))

(define (even-fibs-flow n)
  (accumulate cons
              nil
              (filter even?
                      (map fib
                           (enumerate-interval 0 n)))))

(define (list-fib-squares n)
  (accumulate cons
              nil
              (map square
                   (map fib
                        (enumerate-interval 0 n)))))

(list-fib-squares 10) ;;  {0 1 4 9 16 25 36 49 64 81 100}

(define (product-of-squares-of-odd-elements sequence)
  (accumulate *
              1
              (map square
                   (filter odd? sequence))))

(product-of-squares-of-odd-elements (list 1 2 3 4 5)) ;;  225

;; Exercise 2.33

(define (mapp p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (appendd seq1 seq2)
  (accumulate cons seq2 seq1))

(define (lengthh sequence)
  (accumulate (lambda (item acc) (+ 1 acc)) 0 sequence))

;; Exercise 2.34

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))      ;;  79

;; Exercise 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(accumulate-n + 0 (list (list 1 2 3)
                        (list 4 5 6)
                        (list 7 8 9)
                        (list 10 11 12))) ;;  {22 26 30}

(define (mapn op . seqs)
  (if (null? (car seqs))
      nil
      (cons (apply op (map car seqs))
            (apply mapn op (map cdr seqs)))))

;; Exercise 2.37

(define vector-1 (list 1 2 3))
(define vector-2 (list 4 5 6))
(define matrix-1
  (list (list 1)
        (list 2)
        (list 3)))
(define matrix-2
  (list (list 1 2)
        (list 3 4)
        (list 5 6)))
(define matrix-3
  (list (list 0 2)
        (list 2 0)))

(define (dot-product v w)
  (accumulate + 0 (mapn * v w)))

(+ (* 1 4) (* 2 5) (* 3 6)) ;;  32
(dot-product vector-1 vector-2) ;;  32

(define (transpose mat)
  (accumulate-n cons (list) mat))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) (transpose m)))


(transpose matrix-1) ;;   {{1 2 3}}
(transpose matrix-2)                    ;;   {{1 3 5} {2 4 6}}
(transpose (transpose matrix-2))        ;;   {{1 2} {3 4} {5 6}}
(matrix-*-vector matrix-1 vector-1)

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row)
           (map (lambda (col)
                  (dot-product row col))
                cols))
         m)))

(list (list (+ (* 0 0) (* 2 2))
            (+ (* 0 2) (* 2 0)))
      (list (+ (* 2 0) (* 2 0))
            (+ (* 2 2) (* 0 0))))       ;;  {{4 0} {0 4}}
(matrix-*-matrix matrix-3 matrix-3)     ;;  {{4 0} {0 4}}

;; now we can do matrix multiplication we can finally do ...
;; SUPER FIBONACCI WITH THE Q-MATRIX!!!

(define q-matrix
  (list (list 1 1)
        (list 1 0)))

(define (matrix-square matrix)
  (matrix-*-matrix matrix matrix))

(define range enumerate-interval)

(define (identity-row length i)
  (map (lambda (n)
         (if (= n i) 1 0))
       (range 0 (- length 1))))

(define (identity-matrix-of matrix)
  (let ((l (length matrix)))
    (map (lambda (n) (identity-row l n)) (range 0 (- l 1)))))

(define (matrix-expt matrix n)
  (define (iter n m acc)
    (cond ((= n 0) acc)
	  ((even? n) (iter (/ n 2) (matrix-square m) acc))
	  (else (iter (- n 1) m (matrix-*-matrix m acc)))))
  (iter n matrix (identity-matrix-of matrix)))

;; behold matrix fibonacci
(define (m-fib n)
  (cadar (matrix-expt q-matrix n)))

;; and without the matrix
(define (ff-fib n)
  (define (iter n)
    (if (= n 0) (cons 0 1)
        (let ((pair (iter (quotient n 2))))
          (let ((a (car pair))
                (b (cdr pair)))
            (let ((c (* a (- (* 2 b)
                             a)))
                  (d (+ (* a a) (* b b))))
              (if (even? n)
                  (cons c d)
                  (cons d (+ c d))))))))
  (car (iter n)))

;; Exercise 2.38
(define (fold-left op initial seq)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial seq))
(define fold-right accumulate)

;; 3/2
(fold-right / 1 (list 1 2 3))
;; 1/6
(fold-left / 1 (list 1 2 3))
;;(1 (2 (3 ())))
(fold-right list nil (list 1 2 3))
;; (((() 1) 2) 3)
(fold-left list nil (list 1 2 3))

;; Op must be _commutative_ to give the same result in fold-left and fold-right
;; https://en.wikipedia.org/wiki/Commutative_property

;; Exercise 2.39
(define (reverse-fr seq)
  (fold-right (lambda (i acc)
                (append acc (list i)))
              nil seq))

(define (reverse-fl seq)
  (fold-left (lambda (acc i)
               (cons i acc))
             nil seq))

(reverse-fr (list 1 2 3))

(reverse-fl (list 1 2 3))

;; Bleurgh fold right bleurgh the most evil of all folds BOO

;; Nested Mappings

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))

(prime-sum-pairs 7)
;;  {{2 1 3} {3 2 5} {4 1 5} {4 3 7} {5 2 7} {6 1 7} {6 5 11} {7 4 11} {7 6 13}}

(define (remove item seq)
  (filter (lambda (x) (not (= item x)))
          seq))

(define (permutations s)
  (if (null? s)
      (list nil)
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(permutations (list 1 2 3))
;;  {{1 2 3} {1 3 2} {2 1 3} {2 3 1} {3 1 2} {3 2 1}}

;; Exercise 2.40

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (range 1 (- i 1))))
           (range 1 n)))

(define (simpler-prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

(simpler-prime-sum-pairs 7)
;;  {{2 1 3} {3 2 5} {4 1 5} {4 3 7} {5 2 7} {6 1 7} {6 5 11} {7 4 11} {7 6 13}}

;; Exercise 2.41

(define (triplets n)
  (flatmap (lambda (i)
         (flatmap (lambda (j)
                (map (lambda (k)
                       (list i j k))
                     (range 1 n)))
              (range 1 n)))
       (range 1 n)))

(define (distinct-triple? t)
  (let ((a (car t))
        (b (cadr t))
        (c (caddr t)))
    (and (not (= a b))
         (not (= a c))
         (not (= b c)))))


(define (triple-sums-to? t s)
  (= s (apply + t)))

(define (all-triples-sum-to n s)
  (filter (lambda (t) (triple-sums-to? t s))
          (filter distinct-triple?
                  (triplets n))))

(all-triples-sum-to 5 6)

;;  Exercise 2.42
(define (any? pred seqs)
  (cond ((null? seqs) false)
        ((pred (car seqs)) true)
        (else (any? pred (cdr seqs)))))
(define (remove-p pred seqs)
  (if (null? seqs)
      (list)
      (accumulate (lambda (item acc)
                    (cond ((null? item) acc)
                          ((pred item) acc)
                          (else (cons item acc))))
       (list) seqs)))
(define (first pred seqs)
  (cond ((null? seqs) nil)
        ((pred (car seqs)) (car seqs))
        (else (first pred (cdr seqs)))))
(define (abs-diff n1 n2)
  (max (- n1 n2) (- n2 n1)))

(define empty-board (list))
(define (make-queen rank file)
  (list rank file))
(define (queen-rank queen)
  (car queen))
(define (queen-file queen)
  (cadr queen))
(define (on-same-rank? q1 q2)
  (= (queen-rank q1) (queen-rank q2)))

(define (on-same-file? q1 q2)
  (= (queen-file q1) (queen-file q2)))
(define (queen-eq? q1 q2)
  (and (on-same-file? q1 q2)
       (on-same-rank? q1 q2)))
(define (on-same-diagonal? q1 q2)
  (= (abs-diff (queen-rank q1) (queen-rank q2))
     (abs-diff (queen-file q1) (queen-file q2))))

(define (place-queen file rank board)
  (cons (make-queen rank file) board))

(define (queen-at-rank rank board)
  (first (lambda (queen) (= (queen-rank queen) rank)) board))

(define (safe? rank board)
  (let* ((queen (queen-at-rank rank board))
         (other-queens (remove-p (lambda (q) (queen-eq? q queen)) board)))
    (and (not (any? (lambda (q) (on-same-rank? q queen)) other-queens))
         (not (any? (lambda (q) (on-same-file? q queen)) other-queens))
         (not (any? (lambda (q) (on-same-diagonal? q queen)) other-queens)))))

(define (queens board-size)
  (define (queen-cols rank)
    (if (= rank 0)
        (list empty-board)
        (filter
         (lambda (positions)
           (safe? rank positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (file)
                   (place-queen file rank rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- rank 1))))))
  (queen-cols board-size))

(define (display-row len q)
  (newline)
  (for-each (lambda (pos)
              (if (= pos (queen-file q))
                  (display "|q")
                  (display "| ")))
            (range 1 len))
  (display "|"))

(define (display-board board)
  (let ((len (length board)))
    (for-each (lambda (q) (display-row len q))
              board)))
