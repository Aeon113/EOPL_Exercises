#lang eopl
(define duple
  (lambda (count val)
    (letrec
        ([aux
          (lambda (count already-built)
            (if (zero? count)
               already-built
               (aux (- count 1) (cons val already-built))))])
      (aux count `()))))