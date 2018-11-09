#lang eopl

; ---For procedures---
(define proc?
  (lambda (val)
    (procedure? val)))

(define procedure
  (lambda (var body env)
    (lambda (val)
      (value-of body (extend-env var val env)))))

(define apply-procedure
  (lambda (proc1 val)
    (proc1 val)))

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
    [Exp ("let" id "=" Exp "in" Exp) let-exp]
    [Exp ("proc" "(" id ")" Exp) proc-exp]
    [Exp ("(" Exp Exp ")") call-exp]
    [Exp ("letrec" id "(" id ")" "=" Exp "in" Exp) letrec-exp]
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

(define (extend-env-rec proc-name bound-var proc-body env)
  (lambda (x)
    (if (eqv? x proc-name)
       (procedure bound-var proc-body (extend-env-rec proc-name bound-var proc-body env))
       (env x))))

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
    [let-exp (var val body)
             (evaluate-exp
              body
              (extend-env
               var
               (evaluate-exp val env)
               env))]
    [proc-exp (var body)
             (procedure var body env)]
    [call-exp (rator rand)
             (apply-procedure (evaluate-exp rator env) (evaluate-exp rand env))]
    [letrec-exp (proc-name proc-var proc-body letrec-body)
               (evaluate-exp letrec-body
                            (extend-env-rec proc-name proc-var proc-body env))]
    ))

; ---user interface---
(define value-of evaluate-exp)
(define (value-of-program program-str)
  (cases A-Program (scan&parse program-str)
    [a-program (exp)
               (evaluate-exp exp (empty-env))]))