#lang eopl

; ---For procedures---
(define proc?
  (lambda (val)
    (procedure? val)))

(define procedure
  (lambda (var body)
    (lambda (val env)
      (value-of body (extend-env var val env)))))

(define apply-procedure
  (lambda (proc1 val env)
    (proc1 val env)))

; ---For Values---
(define-datatype expval expval?
  [num-val (num number?)]
  [bool-val (bool boolean?)]
  [proc-val (proc proc?)])

; ---grammar and type definations---
(define scanner-spec
  '([white-sp (whitespace) skip]
    [num (digit (arbno digit)) number]
    [id (letter (arbno (or letter digit))) symbol]))

(define grammar-spec
  '([A-Program (Exp) a-program]
    [Exp (num) const-exp]
    [Exp ("-" "(" Exp "," Exp ")") diff-exp]
    [Exp ("zero?" "(" Exp ")") zero?-exp]
    [Exp ("if" Exp "then" Exp "else" Exp) if-exp]
    [Exp (id) var-exp]
    [Exp ("let" (arbno id "=" Exp) "in" Exp) let-exp]
    [Exp ("proc" "(" id ")" Exp) proc-exp]
    [Exp ("(" Exp Exp ")") call-exp]
    ))

(sllgen:make-define-datatypes scanner-spec grammar-spec)
(define scan&parse (sllgen:make-string-parser scanner-spec grammar-spec))

; ---env---
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

; ---evaluators---
(define (evaluate-let-exp vars vals body env)
  (letrec
      ([aux
        (lambda (vars vals current-env)
          (if (null? vars)
             (evaluate-exp body current-env)
             (aux
              (cdr vars)
              (cdr vals)
              (extend-env
               (car vars)
               (evaluate-exp (car vals) env)
               current-env))))])
    (aux vars vals env)))

(define (evaluate-exp exp env)
  (cases Exp exp
    [const-exp (num) num]
    [diff-exp (lhs rhs) (- (evaluate-exp lhs env) (evaluate-exp rhs env))]
    [zero?-exp (x) (eqv? 0 (evaluate-exp x env))]
    [if-exp (exp1 exp2 exp3)
            (evaluate-exp
             (if (evaluate-exp exp1 env) exp2 exp3)
             env)]
    [var-exp (var) (apply-env env var)]
    [let-exp (vars vals body)
             (evaluate-let-exp vars vals body env)]
    [proc-exp (var body)
             (procedure var body)]
    [call-exp (rator rand)
             (apply-procedure (evaluate-exp rator env) (evaluate-exp rand env) env)]
    ))

; ---user interface---
(define value-of evaluate-exp)
(define (value-of-program program-str)
  (cases A-Program (scan&parse program-str)
    [a-program (exp)
               (evaluate-exp exp (empty-env))]))