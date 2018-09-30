#lang eopl
(define up
  (lambda (lst)
    (letrec
        ([aux
          (lambda (remain the-done)
            (if (null? remain)
               the-done
               (cons
                (car remain)
                (aux (cdr remain) the-done))))])
      (if (null? lst)
         lst
         (let ([head (car lst)] [the-done (up (cdr lst))])
           (if (list? head)
              (aux head the-done)
              (cons head the-done)))))))