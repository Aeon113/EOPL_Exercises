#lang eopl
(define product
  (lambda (sos1 sos2)
    (letrec
        ([aux
          (lambda (s from-sos2 the-done)
            (if (null? from-sos2)
               the-done
               (cons
                (list s (car from-sos2))
                (aux s (cdr from-sos2) the-done))))])
      (if (null? sos1)
         sos1
         (aux
          (car sos1)
          sos2
          (product (cdr sos1) sos2))))))