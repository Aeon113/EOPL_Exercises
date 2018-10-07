#lang eopl
(define-datatype binding binding?
  [binding-type (var symbol?) (val (lambda (x) #t))])

(define-datatype env env?
  [env-type (the-list (list-of binding?))])

(define (empty-env) (env-type `()))

(define (extend-env var val the-env)
  (cases env the-env
    [env-type
     (binding-list)
     (env-type (cons (binding-type var val) binding-list))]))

(define (apply-env the-env search-var)
  (letrec
      ([aux
        (lambda
            (binding-list)
          (if (null? binding-list)
              (eopl:error
               `apply-env
               "No such variable: ~A in this environment:~%~A~%"
               search-var
               the-env)
              (cases binding (car binding-list)
                [binding-type
                 (var val)
                 (if (eqv? var search-var)
                     val
                     (aux (cdr binding-list)))])))])
    (cases env the-env
      [env-type
       (binding-list)
       (aux binding-list)])))

(define (has-binding? the-env search-var search-val)
  (letrec
      ([aux
        (lambda (binding-list)
          (if (null? binding-list)
              #f
              (cases binding (car binding-list)
                [binding-type
                 (var val)
                 (if
                  (and
                   (eqv? var search-var)
                   (equal? val search-val))
                  #t
                  (aux (cdr binding-list)))])))])
    (cases env the-env
      [env-type
       (binding-list)
       (aux binding-list)])))

(define the-env
  (extend-env
   `a 0
   (extend-env
    `b 1
    (extend-env
     `c 2
     (extend-env
      `d 3
      (empty-env))))))