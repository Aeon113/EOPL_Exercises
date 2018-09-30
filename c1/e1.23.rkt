#lang eopl
(define list-index
  (lambda (pred lst)
    (letrec
        ([aux
          (lambda (lst-remain index)
            (if (null? lst-remain)
               #f
               (let ([head (car lst-remain)] [remain (cdr lst-remain)])
                 (if (pred head)
                    index
                    (aux remain (+ 1 index))))))])
      (aux lst 0))))