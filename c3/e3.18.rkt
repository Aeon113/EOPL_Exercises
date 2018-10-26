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
    ;[Exp ("list" "(" (seperated-list Exp ",") ")") list-exp]
    [Exp ("cons" "(" Exp "," Exp ")") cons-exp]
    [Exp ("car" "(" Exp ")") car-exp]
    [Exp ("cdr" "(" Exp ")") cdr-exp]
    [Exp ("null?" "(" Exp ")") null?-exp]
    [Exp ("emptylist") emptylist-exp]
    [Exp ("unpack" (arbno id) "=" Exp "in" Exp) unpack-exp]
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
    [cons-exp (head remains)
             (cons (evaluate-exp head env) (evaluate-exp remains env))]
    [car-exp (list-exp)
            (car (evaluate-exp list-exp env))]
    [cdr-exp (list-exp)
            (cdr (evaluate-exp list-exp env))]
    [null?-exp (list-exp) (null? (evaluate-exp list-exp env))]
    [emptylist-exp () (list)]
    [unpack-exp (var-list val body)
               (letrec
                   ([aux
                     (lambda (var-remains val-remains new-env)
                       (cond
                         [(null? var-remains)
                          (if
                           (null? val-remains)
                           new-env
                           (eopl:error `evaluate-exp "Too few variables."))]
                         [(null? val-remains) (eopl:error `evaluate-exp "Too few values")]
                         [else
                          (aux (cdr var-remains) (cdr val-remains) (extend-env (car var-remains) (car val-remains) new-env))]))])
                 (evaluate-exp body (aux var-list (evaluate-exp val env) env)))]
    ))

; ---user interface---
(define (value-of-program program-str)
  (cases A-Program (scan&parse program-str)
    [a-program (exp)
               (evaluate-exp exp (empty-env))]))