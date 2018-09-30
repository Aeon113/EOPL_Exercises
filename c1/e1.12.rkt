#lang eopl
(define subst
  (lambda (new old var)
    (cond
      [(symbol? var) (if (equal? old var) new var)]
      [(null? var) var]
      [else
       (cons
        (subst new old (car var))
        (subst new old (cdr var)))])))