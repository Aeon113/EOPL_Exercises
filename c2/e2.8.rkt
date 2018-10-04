#lang eopl
(define build list)

(define (get-var element) (car element))

(define (get-val element) (cadr element))

(define (empty-env) `())

(define empty-env? null?)

(define (extend-env env var val)
  (cons (build var val) env))

(define (apply-env env var)
  (letrec ([aux
            (lambda (remains)
              (if (empty-env? remains)
                  (eopl:error
                   `apply-env "No such variable: ~A in this env: ~A.~%"
                   var env)
                  (let ([head (car remains)] [rest (cdr remains)])
                    (if (equal? (get-var head) var)
                        (get-val head)
                        (aux rest)))))])
    (aux env)))