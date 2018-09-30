#lang eopl
(define filter-in
  (lambda (pred lst)
    (if (null? lst)
       lst
       (let ([head (car lst)] [remain (cdr lst)])
         (if (pred head)
            (cons head (filter-in pred remain))
            (filter-in pred remain))))))