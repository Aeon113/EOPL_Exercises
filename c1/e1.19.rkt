#lang eopl
(define list-set
  (lambda (lst n x)
    (cond
     [(null? lst) (eopl:error `list-set "n is too large.~%")]
     [(zero? n) (cons x (cdr lst))]
     [else (cons (car lst) (list-set (cdr lst) (- n 1) x))])))
       