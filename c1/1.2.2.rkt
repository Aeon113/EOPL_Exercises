#lang eopl
(define nth-element
  (lambda (lst index)
    (letrec ([aux (lambda (lst i)
    (if (null? lst)
       (eopl:error `nth-element "List too short by ~s elements.~%" (+ index 1))
       (if (equal? 0 i) (car lst) (aux (cdr lst) (- i 1)))))])
      (if (< index 0)
         (eopl:error `nth-element "Cannot index with a negative number.")
         (aux lst index)))))