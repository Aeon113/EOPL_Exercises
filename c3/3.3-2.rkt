#lang eopl

; ---Value defination---
(define-datatype expval expval?
  [num-val (num number?)]
  [bool-val (bool boolean?)]
  [proc-val (proc proc?)])

; ---For procedures---
(define-datatype proc proc?
  [procedure
   (var symbol?)
   (body Exp?)
   (saved-env env?)])

(define apply-procedure
  (lambda (proc1 val)
    (cases proc proc1
      [procedure (var body saved-env)
                (value-of body (extend-env var val saved-env))])))

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
    [Exp ("proc" "(" id ")" Exp) proc-exp]
    [Exp ("(" Exp Exp ")") call-exp]
    ))

(sllgen:make-define-datatypes scanner-spec grammar-spec)
(define scan&parse (sllgen:make-string-parser scanner-spec grammar-spec))

; ---env---
(define (empty-env) '())
(define env? (list-of list?))

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

; ---evaluators and algorithms and extractors---
(define (get-val val)
  (cases expval val
    [num-val (num) num]
    [bool-val (bool) bool]
    [proc-val (proc) proc]))

(define (substract-exp exp1 exp2)
  (num-val
   (-
    (get-val exp1)
    (get-val exp2))))

(define (evaluate-exp exp env)
  (cases Exp exp
    [const-exp (num) (num-val num)]
    [diff-exp (lhs rhs) (substract-exp (evaluate-exp lhs env) (evaluate-exp rhs env))]
    [zero?-exp (x) (bool-val (eqv? 0 (get-val (evaluate-exp x env))))]
    [if-exp (exp1 exp2 exp3)
            (evaluate-exp
             (if (get-val (evaluate-exp exp1 env)) exp2 exp3)
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
    ))

; ---user interface---
(define value-of evaluate-exp)
(define (value-of-program program-str)
  (cases A-Program (scan&parse program-str)
    [a-program (exp)
               (get-val (evaluate-exp exp (empty-env)))]))