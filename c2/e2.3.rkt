#lang eopl
(define (get-value tree)
  (if (equal? (car tree) `one)
      1
      (- (get-value (cadr tree)) (get-value (caddr tree)))))

(define tree-one
  `(one))

(define tree-zero
  `(diff (one) (one)))

(define tree-negative-one
  `(diff (diff (one) (one)) (one)))

(define (zero) tree-zero)

(define (is-zero? tree)
  (if (equal? (car tree) `one)
      #f
      (equal?
       (get-value (cadr tree))
       (get-value (caddr tree)))))

(define (successor tree)
  (list `diff tree tree-negative-one))

(define (predecessor tree)
  (list `diff tree tree-one))

(define (negative tree)
  (list `diff (zero) tree))

(define (diff-tree-plus lhs rhs)
  (list `diff lhs (negative rhs)))