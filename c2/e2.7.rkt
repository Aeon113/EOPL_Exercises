#lang eopl
(define (apply-env env search-var)
  (letrec
      ([aux
        (lambda (remains)
          (cond
            [(eqv? (car remains) `empty-env)
             (eopl:error
              `apply-env "No such variable: ~A in this env: ~A.~%"
              search-var
              env)]
            [(eqv? (car remains) `extend-env)
             (let ([saved-var (cadr remains)]
                   [saved-val (caddr remains)]
                   [saved-env (cadddr remains)])
               (if (eqv? search-var saved-var)
                   saved-val
                   (aux saved-env)))]
            [else
             (eopl:error `apply-env: "Invalid env: ~A.~%" env)]))])
    (aux env)))