#lang eopl
(define scanner-spec
  '([white-sp (whitespace) skip]
    [num (digit (arbno digit)) number]
    [id (letter (arbno letter digit)) symbol]))

(define grammar-spec
  '([Arith-expr ("let" id "=" Arith-expr "in" Arith-expr) let-expr]
    [Arith-expr (Arith-term (arbno Additive-op Arith-term)) op-expr]
    [Arith-term (Arith-factor (arbno Multiplicative-op Arith-factor)) arith-term]
    [Arith-factor (num) const-factor]
    [Arith-factor (id) var-factor]
    [Arith-factor ("(" Arith-expr ")") expr-factor]
    [Additive-op ("+") plus-op]
    [Additive-op ("-") minus-op]
    [Multiplicative-op ("*") mult-op]
    [Multiplicative-op ("/") div-op]))

(sllgen:make-define-datatypes scanner-spec grammar-spec)
(define scan&parse (sllgen:make-string-parser scanner-spec grammar-spec))

(define (additive-op-parser x)
  (cases Additive-op x
    [plus-op () +]
    [minus-op () -]))

(define (multiplicative-op-parser x)
  (cases Multiplicative-op x
    [mult-op () *]
    [div-op () /]))

(define (empty-env) '())

(define (extend-env var val env)
  (cons
   (list var val)
   env))

(define (apply-env env var)
  (if (null? env)
      (eopl:error 'apply-env "No such variable: ~A~%" var)
      (let ([current (car env)] [rest (cdr env)])
        (if (equal? (car current) var)
            (cadr current)
            (apply-env rest var)))))

(define (evaluate-factor x env)
  (cases Arith-factor x
    [const-factor (num) num]
    [var-factor (id) (apply-env env id)]
    [expr-factor (expr) (evaluate-expr expr env)]))

(define (evaluate-term x env)
  (letrec
      ([aux
        (lambda (current op-list rhs-list)
          (if (null? op-list)
              current
              (aux
               ((multiplicative-op-parser (car op-list))
                current
                (evaluate-factor (car rhs-list) env))
               (cdr op-list)
               (cdr rhs-list))))])
    (cases Arith-term x
      [arith-term (head op-list rhs-list)
                  (aux (evaluate-factor head env) op-list rhs-list)])))

(define (evaluate-expr x env)
  (letrec
      ([aux-op-expr
        (lambda (current op-list rhs-list)
          (if (null? op-list)
              current
              (aux-op-expr
               ((additive-op-parser (car op-list)) current (evaluate-term (car rhs-list) env))
               (cdr op-list)
               (cdr rhs-list))))]
       [aux-let-expr
        (lambda
            (var pre-val expr)
          (evaluate-expr
           expr
           (extend-env
            var
            (evaluate-expr pre-val env)
            env)))])
    (cases Arith-expr x
      [let-expr (var pre-val expr)
                (aux-let-expr var pre-val expr)]
      [op-expr (head op-list rhs-list)
               (aux-op-expr
                (evaluate-term head env)
                op-list
                rhs-list)])))

(define (evaluate expr)
  (evaluate-expr expr (empty-env)))

(define (get-val prog)
  (evaluate (scan&parse prog)))