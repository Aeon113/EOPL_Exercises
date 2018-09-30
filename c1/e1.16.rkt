#lang eopl
(define invert
  (lambda (lst)
    (let
        ([check-2list?
          (lambda (x)
            (and (list? x) (equal? 2 (length x))))]
         [invert-aux
          (lambda (lst)
            (cons (cadr lst) (list (car lst))))])
      (if (null? lst)
         lst
         (let
             ([head (car lst)] [rest (cdr lst)])
           (if (check-2list? head)
              (cons (invert-aux head) (invert rest))
              (eopl:error `invert "Not a 2 list: ~A.~%" head)))))))
        