#lang eopl
(define (var-exp x) x)

(define (lambda-exp var lc-exp)
  (list `lambda var lc-exp))

(define (app-exp rator rand)
  (list rator rand))

(define var-exp? symbol?)

(define (lambda-exp? x)
  (and
   (list? x)
   (eqv? 3 (length x))
   (eqv? `lambda (car x))
   (var-exp? (cadr x))
   (lc-exp? (caddr x))))

(define (app-exp? x)
  (and
   (list? x)
   (eqv? 2 (length x))
   (lc-exp? (car x))
   (lc-exp? (cadr x))))

(define (lc-exp? x)
  (or
   (var-exp? x)
   (lambda-exp? x)
   (app-exp? x)))

(define (var-exp->var x) x)

(define (lambda-exp->bound-var x)
  (cadr x))

(define (lambda-exp->body x)
  (caddr x))

(define (app-exp->rator x)
  (car x))

(define (app-exp->rand x)
  (cadr x))

(define occurs-free?
  (lambda (search-var exp)
    (cond
      [(var-exp? exp)
       (eqv? search-var (var-exp->var exp))]
      [(lambda-exp? exp)
       (and
        (not (eqv? search-var (lambda-exp->bound-var exp)))
        (occurs-free? search-var (lambda-exp->body exp)))]
      [else
       (or
        (occurs-free? search-var (app-exp->rator exp))
        (occurs-free? search-var (app-exp->rand exp)))])))

(define lmbd `(lambda a b))
(define appe (list lmbd `a))