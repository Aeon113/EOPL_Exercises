#lang eopl

; --- ExpVal ---
(define-datatype expval expval?
  [num-val (num number?)]
  [bool-val (bool boolean?)]
  [proc-val (var-list (list-of symbol?)) (body Exp?) (saved-env Env?)])

(define (get-val the-exp-val)
  (cases expval the-exp-val
    [num-val (num) num]
    [bool-val (bool) bool]
    [proc-val (var-list body saved-env)
             (lambda (val-list)
               (value-of body (extend-env-batch var-list val-list saved-env)))]))

; --- Env ---
(define Env? list?)

(define (empty-env) '())

(define (extend-env var val env)
  (cons
   (list var (vector val))
   env))

(define (apply-env env var)
  (if (null? env)
     (eopl:error 'apply-env "No such variable: ~A~%" var)
     (let ([piece (car env)])
       (if (eqv? var (car piece))
          (vector-ref (cadr piece) 0)
          (apply-env (cdr env) var)))))

(define (extend-env-batch var-list val-list env)
  (cond
    [(null? var-list)
     (if (null? val-list)
        env
        (eopl:error 'extend-env-batch "Too many vals: %~%A%~" val-list))]
    [(null? val-list)
     (eopl:error 'extend-env-batch "Too few vals.~%")]
    [else
     (extend-env-batch
      (cdr var-list)
      (cdr val-list)
      (extend-env (car var-list) (car val-list) env))]))

(define (extend-env-batch-without-vals var-list env)
  (if (null? var-list)
     env
     (extend-env-batch-without-vals
      (cdr var-list)
      (extend-env (car var-list) 0 env))))

(define (modify-env var val env)
  (letrec
      [(aux
        (lambda (env-rest)
          (if (null? env)
             (eopl:error 'modify-env "No such variable:~%~A~%" var)
             (let ([piece (car env-rest)])
               (if (eqv? var (car piece))
                  (vector-set! (cadr piece) 0 val)
                  (modify-env var val (cdr env-rest)))))))]
    (begin
      (aux env)
      env)))

; --- Parsing ---
(define scanner-spec
  '([white-sp (whitespace) skip]
    [pos-num (digit (arbno digit)) number]
    [neg-num ("-" digit (arbno digit)) number]
    [id (letter (arbno (or letter digit "_"))) symbol]))

(define grammar-spec
  '([A-Program (Exp) a-program]
    [Exp (pos-num) num-exp]
    [Exp (neg-num) num-exp]
    [Exp ("-" "(" Exp "," Exp ")") diff-exp]
    [Exp ("zero?" "(" Exp ")") zero?-exp]
    [Exp ("if" Exp "then" Exp "else" Exp) if-exp]
    [Exp (id) var-exp]
    [Exp ("let" (arbno id "=" Exp) "in" Exp) let-exp]
    [Exp ("proc" "(" (separated-list id ",") ")" Exp) proc-exp]
    [Exp ("(" Exp (arbno Exp) ")") call-exp]
    [Exp ("letrec" (arbno id "(" (separated-list id ",") ")" "=" Exp) "in" Exp) letrec-exp]
    ))

(sllgen:make-define-datatypes scanner-spec grammar-spec)
(define scan&parse (sllgen:make-string-parser scanner-spec grammar-spec))

; --- Evaluators ---
(define (build-env-letrec proc-name-list vars-list body-list current-env)
  (let
      ([tmp-env (extend-env-batch-without-vals proc-name-list current-env)])
    (letrec
        ([build-procs
          (lambda (proc-name-list-remains vars-list-remains body-list-remains)
            (if (null? proc-name-list-remains)
               tmp-env
               (begin
                 (modify-env
                  (car proc-name-list-remains)
                  (proc-val
                   (car vars-list-remains)
                   (car body-list-remains)
                   tmp-env)
                  tmp-env)
               (build-procs
                (cdr proc-name-list-remains)
                (cdr vars-list-remains)
                (cdr body-list-remains)))))])
      (begin
        (build-procs proc-name-list vars-list body-list)
        tmp-env))))

(define (batch-value-of val-list env)
  (if (null? val-list)
     '()
     (cons
      (value-of (car val-list) env)
      (batch-value-of (cdr val-list) env))))

(define (value-of exp env)
  (cases Exp exp
    [num-exp (num) (num-val num)]
    [diff-exp
     (lhs rhs)
     (num-val (- (get-val (value-of lhs env)) (get-val (value-of rhs env))))]
    [zero?-exp (x)
              (bool-val (eqv? 0 (get-val (value-of x env))))]
    [if-exp (exp1 exp2 exp3)
           (value-of
            (if (get-val (value-of exp1 env))
               exp2
               exp3)
            env)]
    [var-exp (id) (apply-env env id)]
    [let-exp (var-list val-list body)
            (value-of
             body
             (extend-env-batch
              var-list
              (batch-value-of val-list env)
              env))]
    [proc-exp (var-list body)
             (proc-val var-list body env)]
    [call-exp (rator rands)
             ((get-val (value-of rator env)) (batch-value-of rands env))]
    [letrec-exp (proc-name-list vars-list proc-body-list body)
               (value-of
                body
                (build-env-letrec proc-name-list vars-list proc-body-list env))]))

; --- User Interface ---
(define (value-of-program program-str)
  (cases A-Program (scan&parse program-str)
    [a-program (exp)
               (get-val (value-of exp (empty-env)))]))