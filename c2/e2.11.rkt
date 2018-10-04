#lang eopl
(define (empty-env) `())

(define empty-env? null?)

(define (extend-env var val env)
  (cons (list (list var) (list val)) env))

(define (apply-env env var)
  (letrec
      ([aux
        (lambda (var-list val-list)
          (cond
            [(null? var-list) (apply-env (cdr env) var)]
            [(equal? var (car var-list)) (car val-list)]
            [else (aux (cdr var-list) (cdr val-list))]))])
    (if (null? env)
        (eopl:error
         `apply-env
         "No such variable: ~A.~%"
         var)
        (let ([head (car env)])
          (aux (car head) (cadr head))))))

(define (extend-env* var-list val-list env)
  (if (eqv? (length var-list) (length val-list))
      (cons (list var-list val-list) env)
      (eopl:error
       `extend-env
       "The lengths of var-list and val-list are not the same.")))