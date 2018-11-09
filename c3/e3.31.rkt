#lang eopl

; ---For procedures---
(define proc?
  (lambda (val)
    (procedure? val)))

(define procedure
  (lambda (vars body env)
    (lambda (vals)
      (value-of body (extend-env-batch vars vals env)))))

(define apply-procedure
  (lambda (proc1 vals)
    (proc1 vals)))

; ---grammar and type definations---
(define scanner-spec
  '([white-sp (whitespace) skip]
    [num (digit (arbno digit)) number]
    [id (letter (arbno (or letter digit))) symbol]))

(define grammar-spec
  '([A-Program (Exp) a-program]
    [Exp ("-" num) negative-const-exp]
    [Exp (num) const-exp]
    [Exp ("-(" Exp "," Exp ")") diff-exp]
    [Exp ("zero?" "(" Exp ")") zero?-exp]
    [Exp ("if" Exp "then" Exp "else" Exp) if-exp]
    [Exp (id) var-exp]
    [Exp ("let" (arbno id "=" Exp) "in" Exp) let-exp]
    [Exp ("proc" "(" (separated-list id ",") ")" Exp) proc-exp]
    [Exp ("(" Exp (arbno Exp) ")") call-exp]
    [Exp ("letrec" id "(" (separated-list id ",") ")" "=" Exp "in" Exp) letrec-exp]
    ))

(sllgen:make-define-datatypes scanner-spec grammar-spec)
(define scan&parse (sllgen:make-string-parser scanner-spec grammar-spec))

; ---env---
(define (empty-env)
  (lambda (x)
    (eopl:error
     'apply-env
     "No such variable: ~A~%"
     x)))

(define (extend-env var val env)
  (lambda (x)
    (if (eqv? x var)
       val
       (env x))))

(define (apply-env env var)
  (env var))

(define (extend-env-rec proc-name bound-vars proc-body env)
  (lambda (x)
    (if (eqv? x proc-name)
       (procedure bound-vars proc-body (extend-env-rec proc-name bound-vars proc-body env))
       (env x))))

(define (extend-env-batch vars vals env)
  (if (null? vars)
     env
     (extend-env
      (car vars)
      (car vals)
      (extend-env-batch
       (cdr vars)
       (cdr vals)
       env))))

; ---evaluators---
(define (evaluate-exp exp env)
  (cases Exp exp
    [negative-const-exp (num) (- num)]
    [const-exp (num) num]
    [diff-exp (lhs rhs) (- (evaluate-exp lhs env) (evaluate-exp rhs env))]
    [zero?-exp (x) (eqv? 0 (evaluate-exp x env))]
    [if-exp (exp1 exp2 exp3)
            (evaluate-exp
             (if (evaluate-exp exp1 env) exp2 exp3)
             env)]
    [var-exp (var) (apply-env env var)]
    [let-exp (vars vals body)
             (evaluate-exp
              body
              (extend-env-batch
               vars
               (evaluate-exp-batch vals env)
               env))]
    [proc-exp (vars body)
             (procedure vars body env)]
    [call-exp (rator rands)
             (apply-procedure (evaluate-exp rator env) (evaluate-exp-batch rands env))]
    [letrec-exp (proc-name proc-vars proc-body letrec-body)
               (evaluate-exp letrec-body
                            (extend-env-rec proc-name proc-vars proc-body env))]
    ))

(define (evaluate-exp-batch exp-list env)
  (if (null? exp-list)
     '()
     (cons
      (evaluate-exp (car exp-list) env)
      (evaluate-exp-batch (cdr exp-list) env))))

; ---user interface---
(define value-of evaluate-exp)
(define (value-of-program program-str)
  (cases A-Program (scan&parse program-str)
    [a-program (exp)
               (evaluate-exp exp (empty-env))]))