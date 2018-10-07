#lang eopl
(define (empty-env)
  (list
   (lambda (search-var)
     (eopl:error
      `apply-env
      "No such variable: ~A.~%"
      search-var))
   (lambda () #t)))

(define (extend-env var val env)
  (list
   (lambda (search-var)
     (if (equal? search-var var)
         val
         ((car env) search-var)))
   (lambda () #f)))

(define (apply-env env var)
  ((car env) var))

(define (empty-env? env)
  ((cadr env)))