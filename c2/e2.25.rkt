#lang eopl
(define-datatype bintree bintree?
  (leaf-node (num integer?))
  (interior-node
   (key symbol?)
   (left bintree?)
   (right bintree?)))

(define-datatype max-val-type max-val-type?
  [max-val (symbol symbol?) (val number?)]
  [not-set-yet (dummy (lambda (X) #t))])

(define (compare-max-vals lhs rhs)
  (cases max-val-type lhs
    [not-set-yet (dummy) rhs]
    [max-val
     (lhs-symbol lhs-val)
     (cases max-val-type rhs
       [not-set-yet (dummy) lhs]
       [max-val
        (rhs-symbol rhs-val)
        (if (< lhs-val rhs-val) rhs lhs)])]))

(define (max-interior tree)
  (letrec
      ([aux ; Its return value is (list val-of-this-node current-max-val).
        (lambda (current-max subtree)
          (cases bintree subtree
            [leaf-node (num)
                       (list num current-max)]
            [interior-node
             (key left right)
             (letrec
                 ([left-result (aux current-max left)]
                  [left-val (car left-result)]
                  [left-max (cadr left-result)]
                  [right-result (aux left-max right)]
                  [right-val (car right-result)]
                  [right-max (cadr right-result)]
                  [children-max right-max]
                  [this-val (+ left-val right-val)]
                  [final-max (compare-max-vals children-max (max-val key this-val))])
               (list this-val final-max))]))])
    (cases max-val-type (cadr (aux (not-set-yet `dummy) tree))
      [not-set-yet
       (dummy)
       (eopl:error `max-interior
                   "Try to get max interior from a leaf:~%~A~%"
                   tree)]
      [max-val (symbol val) symbol])))



; Test case
(define tree-1
    (interior-node `foo (leaf-node 2) (leaf-node 3)))
(define tree-2
    (interior-node `bar  (leaf-node -1) tree-1))
(define tree-3
    (interior-node `baz  tree-2 (leaf-node 1)))
(max-interior tree-2)
(max-interior tree-3)

(define tree-4
  (interior-node
   `max
   (interior-node `shall-not-be-this (leaf-node 9) (leaf-node -5))
   (leaf-node 5)))

(define tree-5
  (interior-node
   `shall-not-be-this
   (leaf-node -17)
   (interior-node
    `max
    (leaf-node 18)
    (leaf-node -99))))
