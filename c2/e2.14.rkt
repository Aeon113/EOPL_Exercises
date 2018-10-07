#lang eopl
(define (empty-env)
  (list
   (lambda (search-var)
     (eopl:error
      `apply-env
      "No such variable: ~A.~%"
      search-var))
   (lambda () #t)
   (lambda (var val)
     #f)))

(define (extend-env var val env)
  (list
   (lambda (search-var)
     (if (equal? search-var var)
         val
         ((car env) search-var)))
   (lambda () #f)
   (lambda (search-var search-val)
     (if
      (and
       (equal? var search-var)
       (equal? val search-val))
      #t
      ((caddr env) search-var search-val)))))

(define (apply-env env var)
  ((car env) var))

(define (empty-env? env)
  ((cadr env)))

(define (has-binding? env var val)
  ((caddr env) var val))

