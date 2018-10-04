#lang eopl
(define (has-binding? env var val)
  (let ([head (car env)])
    (cond
      [(eqv? head `extend-env)
       (if (and (eqv? var (cadr env)) (eqv? val (caddr env)))
           #t
           (has-binding? (cadddr env) var val))]
      [(eqv? head `empty-env) #f]
      [else
       (eopl:error `has-binding?
                   "Invalid env.~%")])))