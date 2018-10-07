#lang eopl
(define identifier? symbol?)

(define-datatype lc-exp lc-exp?
  (var-exp (var identifier?))
  (lambda-exp
   (bound-var (lambda (x) (and (identifier? x) (not (eqv? x `lambda)))))
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand lc-exp?)))