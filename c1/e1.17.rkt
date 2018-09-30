#lang eopl
(define down
  (lambda (lst)
    (if (null? lst)
       lst
       (cons (list (car lst)) (down (cdr lst))))))