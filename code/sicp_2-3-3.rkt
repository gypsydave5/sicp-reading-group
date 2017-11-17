#lang sicp

;; 2.3.3 Example: Representing Sets

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      (set)
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;;* Set examples
;;** Init set1:
(define set1 '(1 2 3))
;;** Init set2:
(define set2 '(2 3 4))
;;** element-of-set? :
(element-of-set? 1 set1)
(adjoin-set 0 set1)
(intersection-set set1 set2)
(union-set set1 set2)
;; Exercise 2.59

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (cons (car set1)
                    (union-set (cdr set1) set2)))))

;; Exercise 2.60 - duplicate representation

(define (element-of-set?-dup x set)
  (element-of-set? x set))

(define (adjoin-set-dup x set)
  (cons x set))

(define (union-set-dup set1 set2)
  (append set1 set2))

(define (intersection-set-dup set1 set2 )
  (intersection-set set1 set2))

;; The space gets large, so while `adjoin` and `union` become
;; cheaper, `intersection` now gets more expensive (as the `element-of`
;; operation is having to iterate over the much longer lists).

;; Ordered representation
(define (element-of-set?-o x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

;; Still Θn, but on average examines half the elements of a set

(define (intersection-set-o set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set-o (cdr set1) (cdr set2)))
               ((< x1 x2)
                (intersection-set-o (cdr set1) set2))
               ((< x2 x1)
                (intersection-set-o set1 (cdr set2))))))))

;; As the total number of steps is now (at most) `(+ (length set1) (length set2))` this
;; now takes Θn, rather than Θn².

;; Exercise 2.61
(define (adjoin-set-o x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< (car set) x) (cons (car set) (cons x (cdr set))))
        (else (cons (car set) (adjoin-set-o x (cdr set))))))

;; Exercise 2.62
(define (union-set-o set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((first1 (car set1)) (first2 (car set2)))
                (cond ((= first1 first2)
                       (cons first1 (union-set-o (cdr set1) (cdr set2))))
                      ((< first1 first2)
                       (cons first1 (cons first2 (union-set-o (cdr set1) (cdr set2)))))
                      ((< first2 first1)
                       (cons first2 (cons first1 (union-set-o (cdr set1) (cdr set2))))))))))


;; Sets as binary trees

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set?-t x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set?-t x (left-branch set)))
        ((> x (entry set))
         (element-of-set?-t x (right-branch set)))))

(define (adjoin-set-t x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set-t x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set-t x (right-branch set))))))

;; Exercise 2.63

(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

;; Exercise 2.64

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  "Partial tree generates a tree from the first n elements of elts
   and returns a pair of the tree and the remaining elts. The tree is
   balanced."
  (if (= n 0)
      (cons '() elts)
      (let* ((left-size (quotient (- n 1) 2)) ;; I'm cheating using *let!
             (left-result (partial-tree elts left-size)) ;; recursion!
             (left-tree (car left-result))
             (non-left-elts (cdr left-result))
             (right-size (- n (+ left-size 1)))
             (this-entry (car non-left-elts))
             (right-result (partial-tree (cdr non-left-elts) right-size)) ;; recursion!
             (right-tree (car right-result))
             (remaining-elts (cdr right-result)))
        (cons (make-tree this-entry left-tree right-tree)
              remaining-elts))))

;; b. log(n)

;; Exercise 2.65
(define (union-set-t set1 set2)
  (list->tree (union-set-o (tree->list set1))
              (union-set-o (tree->list set2))))

(define (intersection-set-t set1 set2)
  (list->tree (intersection-set-o (tree->list set1))
              (intersection-set-o (tree->list set2))))

;; Sets and Information retrieval
;; Exercise 2.66
;; I cannot be bother'd
