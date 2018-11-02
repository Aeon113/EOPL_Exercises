#lang eopl

; ---Value Definations---
(define-datatype expval expval?
  [num-val (num number?)]
  [bool-val (bool boolean?)]
  [proc-val (proc procedure?)])

(define (extract-expval x)
  (cases expval x
    [num-val (num) num]
    [bool-val (bool) bool]
    [proc-val (proc) proc]))

; --- Scanner and Grammar Spec ---
(define scanner-spec
  '([white-sp (whitespace) skip]
    [num (digit (arbno digit)) number]
    [id ((or letter "+" "-" "?") (arbno (or letter digit "+" "-" "?"))) symbol]))

(define grammar-spec
  '([A-Program (Exp) a-program]
    [Exp (num) const-exp]
    [Exp ("if" Exp "then" Exp "else" Exp) if-exp]
    [Exp (id) var-exp]
    [Exp ("let" id "=" Exp "in" Exp) let-exp]
    [Exp ("proc" "(" (arbno id) ")" Exp) proc-exp]
    [Exp ("(" Exp (arbno Exp) ")") call-exp]
    ))

(sllgen:make-define-datatypes scanner-spec grammar-spec)
(define scan&parse (sllgen:make-string-parser scanner-spec grammar-spec))

; ---Initial Procs---
(define sub-proc
  (proc-val
   (lambda
       (numval-list)
     (if
      (eqv? 2 (length numval-list))
      (num-val (- (extract-expval (car numval-list)) (extract-expval (cadr numval-list))))
      (eopl:error
       'call-exp
       "Number of parameters not match:~%Expected: ~A~%Given: ~A~%" 2
       (length numval-list))))))

(define add-proc
  (proc-val
   (lambda
       (numval-list)
     (if
      (eqv? 2 (length numval-list))
      (num-val (+ (extract-expval (car numval-list)) (extract-expval (cadr numval-list))))
      (eopl:error
       'call-exp
       "Number of parameters not match:~%Expected: ~A~%Given: ~A~%" 2
       (length numval-list))))))

(define zero?-proc
  (proc-val
   (lambda
       (val-list)
     (if
      (eqv? 1 (length val-list))
      (bool-val (eqv? 0 (extract-expval (car val-list))))
      (eopl:error
       'call-exp
       "Number of parameters not match:~%Expected: ~A~%Given: ~A~%" 1
       (length val-list))))))

; --- Env ---
(define (empty-env) '())

(define (extend-env var val env)
  (cons (list var val) env))

(define (apply-env env var)
  (cond
    [(null? env) (eopl:error 'apply-env "No such variable: ~A~%" var)]
    [(eqv? var (car (car env))) (cadr (car env))]
    [else (apply-env (cdr env) var)]))

(define initial-env
  (extend-env '- sub-proc
   (extend-env '+ add-proc
    (extend-env 'zero? zero?-proc
     (empty-env)))))

; ---evaluators---
(define (build-evaluated-rands-list rands-list env)
  (if (null? rands-list)
     '()
     (cons
       (evaluate-exp (car rands-list) env)
       (build-evaluated-rands-list (cdr rands-list) env))))

(define (build-proc var-list body current-env)
  (letrec
      ([var-count (length var-list)]
       [build-env
        (lambda (var-remain val-remain)
          (if (null? var-remain)
             current-env
             (extend-env
              (car var-remain)
              (car val-remain)
              (build-env
               (cdr var-remain)
               (cdr val-remain)))))]
       [count-check
        (lambda (val-list)
          (eqv? var-count (length val-list)))])
    (proc-val (lambda (val-list)
      (if (count-check val-list)
         (evaluate-exp body (build-env var-list val-list))
         (eopl:error
          'call-exp
          "Number of parameters not match:~%Expected: ~A~%Given: ~A~%"
          var-count
          (length val-list)))))))
         

(define (evaluate-exp exp env)
  (cases Exp exp
    [const-exp (num) (num-val num)]
    [if-exp (exp1 exp2 exp3)
           (evaluate-exp
            (if (extract-expval (evaluate-exp exp1 env))
               exp2
               exp3)
            env)]
    [var-exp (id) (apply-env env id)]
    [let-exp (id val body)
            (evaluate-exp
             body
             (extend-env id (evaluate-exp val env) env))]
    [proc-exp (var-list body) (build-proc var-list body env)]
    [call-exp (rator rands)
             ((extract-expval (evaluate-exp rator env)) (build-evaluated-rands-list rands env))]))

; ---User Interface---
(define (value-of-program program-str)
  (cases A-Program (scan&parse program-str)
    [a-program (exp)
               (extract-expval (evaluate-exp exp initial-env))]))