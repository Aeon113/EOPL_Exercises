#lang eopl
(define (lambda-var? x)
  (and (symbol? x) (not (eqv? x `lambda))))

(define-datatype lc-exp lc-exp?
  [var-exp (var symbol?)]
  [lambda-exp (bound-vars (list-of lambda-var?)) (body lc-exp?)]
  [app-exp (rator lc-exp?) (rands (list-of lc-exp?))])

(define (parse exp)
  (letrec
      ([parse-list
        (lambda (remains)
          (if (null? remains)
             (list)
             (cons (parse (car remains)) (parse-list (cdr remains)))))])
    (cond
      [(symbol? exp) (var-exp exp)]
      [(= 2 (length exp)) (app-exp (parse (car exp)) (parse-list (cadr exp)))]
      [(eqv? `lambda (car exp)) (lambda-exp (cadr exp) (parse (caddr exp)))]
      [else (eopl:error `parse "Cannot parse this exp:~%~A~%" exp)])))

(define (unparse exp)
  (letrec
      ([unparse-list
        (lambda (remains)
          (if (null? remains)
             (list)
             (cons (unparse (car remains)) (unparse-list (cdr remains)))))])
    (cases lc-exp exp
      [var-exp (var) var]
      [lambda-exp (bound-vars body) (list `lambda bound-vars (unparse body))]
      [app-exp (rator rands) (list (unparse rator) (unparse-list rands))])))

; Test case
(define to-parse `((lambda (a b c) c) (a b c d)))