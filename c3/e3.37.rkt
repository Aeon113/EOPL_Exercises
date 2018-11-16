#lang eopl

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
    [Exp (">" "(" Exp "," Exp ")") gt-exp]
    [Exp ("<" "(" Exp "," Exp ")") ls-exp]
    [Exp ("=" "(" Exp "," Exp ")") eq-exp]
    [Exp (">=" "(" Exp "," Exp ")") ge-exp]
    [Exp ("<=" "(" Exp "," Exp ")") le-exp]
    [Exp ("cond" (arbno Exp "==>" Exp) "end") cond-exp]
    ))

(sllgen:make-define-datatypes scanner-spec grammar-spec)
(define scan&parse (sllgen:make-string-parser scanner-spec grammar-spec))

; --- Nameless Value Datatype definations ---
(define-datatype n-exp n-exp?
  [n-num-exp (num number?)]
  [n-diff-exp
   (lhs n-exp?)
   (rhs n-exp?)]
  [n-zero?-exp (the-exp n-exp?)]
  [n-if-exp
   (exp1 n-exp?)
   (exp2 n-exp?)
   (exp3 n-exp?)]
  [n-var-exp (index number?)]
  [n-let-exp
   (vals (list-of n-exp?))
   (body n-exp?)]
  [n-proc-exp
   (var-count number?)
   (body n-exp?)]
  [n-call-exp
   (rator n-exp?)
   (rands (list-of n-exp?))]
  [n-gt-exp (lhs n-exp?) (rhs n-exp?)]
  [n-ls-exp (lhs n-exp?) (rhs n-exp?)]
  [n-eq-exp (lhs n-exp?) (rhs n-exp?)]
  [n-ge-exp (lhs n-exp?) (rhs n-exp?)]
  [n-le-exp (lhs n-exp?) (rhs n-exp?)]
  [n-cond-exp (cond-list (list-of n-exp?)) (body-list (list-of n-exp?))]
  )

; --- expval ---
(define-datatype expval expval?
  [num-val (num number?)]
  [bool-val (bool boolean?)]
  [proc-val (param-count number?) (body n-exp?) (saved-env nenv?)])

(define (extract-expval val)
  (cases expval val
    [num-val (num) num]
    [bool-val (bool) bool]
    [proc-val
     (param-count body saved-env)
     val]))

; --- Static Environment ---
(define report-unbound-var
  (lambda (var)
    (eopl:error
     'apply-senv
     "No such variable: ~A~%"
     var)))

(define senv? (list-of symbol?))

(define empty-senv
  (lambda ()
    '()))

(define extend-senv
  (lambda (var senv)
    (cons var senv)))

;       Right first
(define batch-extend-senv
  (lambda (var-list senv)
    (if (null? var-list)
       senv
       (batch-extend-senv (cdr var-list) (extend-senv (car var-list) senv)))))

(define apply-senv
  (lambda (senv var)
    (cond
      [(null? senv) (report-unbound-var var)]
      [(eqv? var (car senv)) 0]
      [else (+ 1 (apply-senv (cdr senv) var))])))

; --- Namelss Environment ---
(define nenv? (list-of vector?))

(define empty-nenv
  (lambda () '()))

(define extend-nenv
  (lambda (var nenv)
    (cons (vector var) nenv)))

    ;Right first
(define batch-extend-nenv
  (lambda (val-list nenv)
    (if (null? val-list)
       nenv
       (batch-extend-nenv (cdr val-list) (extend-nenv (car val-list) nenv)))))

(define apply-nenv
  (lambda (nenv index)
    (vector-ref (list-ref nenv index) 0)))

(define modify-nenv
  (lambda (nenv index new-val)
    (vector-set! (list-ref nenv index) 0 new-val)))

; --- Translation ---
    ; --- Left first ---
(define batch-translation-of
  (lambda (exp-list senv)
    (if (null? exp-list)
       '()
       (cons
        (translation-of (car exp-list) senv)
        (batch-translation-of (cdr exp-list) senv)))))

(define translation-of
  (lambda (exp senv)
    (cases Exp exp
      [num-exp (num) (n-num-exp num)]
      [diff-exp (lhs rhs)
                 (n-diff-exp
                  (translation-of lhs senv)
                  (translation-of rhs senv))]
      [zero?-exp (the-exp)
                (n-zero?-exp
                 (translation-of the-exp senv))]
      [if-exp (exp1 exp2 exp3)
             (n-if-exp
              (translation-of exp1 senv)
              (translation-of exp2 senv)
              (translation-of exp3 senv))]
      [var-exp (id) (n-var-exp (apply-senv senv id))]
      [let-exp (name-list vals body)
              (n-let-exp
               (batch-translation-of vals senv)
               (translation-of body (batch-extend-senv name-list senv)))]
      [proc-exp (param-list body)
               (n-proc-exp
                (length param-list)
                (translation-of body (batch-extend-senv param-list senv)))]
      [call-exp (rator rands)
               (n-call-exp
                (translation-of rator senv)
                (batch-translation-of rands senv))]
      [gt-exp (lhs rhs)
             (n-gt-exp
              (translation-of lhs senv)
              (translation-of rhs senv))]
      [ls-exp (lhs rhs)
             (n-ls-exp
              (translation-of lhs senv)
              (translation-of rhs senv))]
      [eq-exp (lhs rhs)
             (n-eq-exp
              (translation-of lhs senv)
              (translation-of rhs senv))]
      [ge-exp (lhs rhs)
             (n-ge-exp
              (translation-of lhs senv)
              (translation-of rhs senv))]
      [le-exp (lhs rhs)
             (n-le-exp
              (translation-of lhs senv)
              (translation-of rhs senv))]
      [cond-exp (cond-list body-list)
               (n-cond-exp
                (batch-translation-of cond-list senv)
                (batch-translation-of body-list senv))]
      )))

; --- Evaluators ---
(define nameless-cond-evaluator
  (lambda (conds bodies nenv)
    (letrec
        ([aux
          (lambda (conds-remain bodies-remain)
            (cond
              [(null? conds-remain)
               (eopl:error
                'nameless-cond-evaluator
                "No condition is evaluated to true:~%~A~%" conds)]
              [(extract-expval (value-of (car conds-remain) nenv))
               (value-of (car bodies-remain) nenv)]
              [else (aux (cdr conds-remain) (cdr bodies-remain))]))])
      (aux conds bodies))))

(define nameless-call
  (lambda (rator rands)
    (cases expval rator
      [proc-val (var-count body saved-env)
                 (let
                     ([rands-check
                       (lambda ()
                         (cond
                           [(not ((list-of expval?) rands))
                            (eopl:error
                             'nameless-call
                             "rands is a list of expval:~%~A~%"
                             rands)]
                           [(not (eqv? var-count (length rands)))
                            (eopl:error
                             'apply-nameless-proc
                             "Procedure Parameters Arity Mismatch: Expected: ~A\tGiven: ~A~%rator:~%~A~%rands:~%~A~%"
                             var-count (length rands) rator rands)]))])
                   (begin
                     (rands-check)
                     (value-of body (batch-extend-nenv rands saved-env))))]
      [else
       (eopl:error 'nameless-call
                  "The rator is not a nameless-procedure:~%~A~%" rator)])))

    ; --- left First ---
(define batch-value-of
  (lambda (n-exp-list nenv)
    (if (null? n-exp-list)
       '()
       (cons (value-of (car n-exp-list) nenv) (batch-value-of (cdr n-exp-list) nenv)))))

(define value-of
  (lambda (exp nenv)
    (cases n-exp exp
      [n-num-exp (num) (num-val num)]
      [n-diff-exp (lhs rhs)
                 (num-val
                  (-
                   (extract-expval (value-of lhs nenv))
                   (extract-expval (value-of rhs nenv))))]
      [n-zero?-exp (the-exp)
                  (bool-val (eqv? 0 (extract-expval (value-of the-exp nenv))))]
      [n-if-exp (exp1 exp2 exp3)
               (value-of
                (if (extract-expval (value-of exp1 nenv)) exp2 exp3)
                nenv)]
      [n-var-exp (index) (apply-nenv nenv index)]
      [n-let-exp (vals body)
                (value-of body (batch-extend-nenv (batch-value-of vals nenv) nenv))]
      [n-proc-exp (var-count body)
                 (proc-val var-count body nenv)]
      [n-call-exp (rator rands)
                 (nameless-call
                  (value-of rator nenv)
                  (batch-value-of rands nenv))]
      [n-gt-exp (lhs rhs)
               (bool-val
                (>
                 (extract-expval (value-of lhs nenv))
                 (extract-expval (value-of rhs nenv))))]
      [n-ls-exp (lhs rhs)
               (bool-val
                (<
                 (extract-expval (value-of lhs nenv))
                 (extract-expval (value-of rhs nenv))))]
      [n-eq-exp (lhs rhs)
               (bool-val
                (eqv?
                 (extract-expval (value-of lhs nenv))
                 (extract-expval (value-of rhs nenv))))]
      [n-ge-exp (lhs rhs)
               (bool-val
                (>=
                 (extract-expval (value-of lhs nenv))
                 (extract-expval (value-of rhs nenv))))]
      [n-le-exp (lhs rhs)
               (bool-val
                (<=
                 (extract-expval (value-of lhs nenv))
                 (extract-expval (value-of rhs nenv))))]
      [n-cond-exp (conds bodies)
                 (nameless-cond-evaluator
                  conds
                  bodies
                  nenv)]
      )))

; --- User Interfaces ---
(define value-of-program
  (lambda (str)
    (let ([ast (scan&parse str)])
      (cases A-Program ast
        [a-program (exp)
                  (let ([translation (translation-of exp (empty-senv))])
                    (let ([value (value-of translation (empty-nenv))])
                      (extract-expval value)))]))))