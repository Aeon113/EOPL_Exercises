#lang eopl

; ---grammar and type definations---
(define scanner-spec
  '([white-sp (whitespace) skip]
    [num (digit (arbno digit)) number]
    [id (letter (arbno (or letter digit))) symbol]))

(define grammar-spec
  '([A-Program (Exp) a-program]
    [Exp (num) const-exp]
    [Exp ("-" "(" Exp "," Exp ")") diff-exp]
    [Bool-Exp ("zero?" "(" Exp ")") zero?-exp]
    [Exp ("if" Bool-Exp "then" Exp "else" Exp) if-exp]
    [Exp (id) var-exp]
    [Exp ("let" id "=" Exp "in" Exp) let-exp]))

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
(define (value-of-bool-exp exp env)
  (cases Bool-Exp exp
    [zero?-exp (the-exp)
              (eqv? 0 (evaluate-exp the-exp env))]))

(define (evaluate-exp exp env)
  (cases Exp exp
    [const-exp (num) num]
    [diff-exp (lhs rhs) (- (evaluate-exp lhs env) (evaluate-exp rhs env))]
    [if-exp (exp1 exp2 exp3)
            (evaluate-exp
             (if (value-of-bool-exp exp1 env) exp2 exp3)
             env)]
    [var-exp (var) (apply-env env var)]
    [let-exp (var val body)
             (evaluate-exp
              body
              (extend-env
               var
               (evaluate-exp val env)
               env))]))

; ---user interface---
(define (value-of-program program-str)
  (cases A-Program (scan&parse program-str)
    [a-program (exp)
               (evaluate-exp exp (empty-env))]))