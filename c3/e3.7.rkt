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
    [Exp ("zero?" "(" Exp ")") zero?-exp]
    [Exp ("if" Exp "then" Exp "else" Exp) if-exp]
    [Exp (id) var-exp]
    [Exp ("let" id "=" Exp "in" Exp) let-exp]
    [Exp ("+" "(" Exp "," Exp ")") add-exp]
    [Exp ("*" "(" Exp "," Exp ")") mult-exp]
    [Exp ("quotient" "(" Exp "," Exp ")") quo-exp]
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
    [let-exp (var val body)
             (evaluate-exp
              body
              (extend-env
               var
               (evaluate-exp val env)
               env))]
    [add-exp (lhs rhs)
            (+ (evaluate-exp lhs env) (evaluate-exp rhs env))]
    [mult-exp (lhs rhs)
             (* (evaluate-exp lhs env) (evaluate-exp rhs env))]
    [quo-exp (lhs rhs)
            (quotient (evaluate-exp lhs env) (evaluate-exp rhs env))]
    ))

; ---user interface---
(define (value-of-program program-str)
  (cases A-Program (scan&parse program-str)
    [a-program (exp)
               (evaluate-exp exp (empty-env))]))