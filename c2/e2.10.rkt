#lang eopl
(define (extend-env* var-list val-list env)
  (letrec
      ([aux
        (lambda (var-remains val-remains)
          (if (null? var-remains)
              env
              (list
               `extend-env
               (car var-remains)
               (car val-remains)
               (aux
                (cdr var-remains)
                (cdr val-remains)))))])
    (if (eqv? (length var-list) (length val-list))
        (aux var-list val-list)
        (eopl:error
         `extend-env*
         "The lengths of var-list and val-list are not the same."))))