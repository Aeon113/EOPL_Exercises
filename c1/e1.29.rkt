#lang eopl
(define sort
  (lambda (lst)
    (letrec
        ([aux
          (lambda (l)
            (if (eq? 1 (length l))
                l
                (let ([result (aux (cdr l))])
                  (let
                      ([current-head (car l)]
                       [result-head (car result)]
                       [result-rest (cdr result)])
                    (if (< current-head result-head)
                        (cons current-head result)
                        (cons result-head (cons current-head result-rest)))))))])
      (if (null? lst)
          lst
          (let ([result (aux lst)])
            (cons (car result) (sort (cdr result))))))))