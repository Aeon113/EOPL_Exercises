#lang eopl
(define-datatype lc-exp lc-exp?
  [var-exp (var symbol?)]
  [lambda-exp
   (bound-var (lambda (x) (and (symbol? x) (not (eqv? x `lambda)))))
   (body lc-exp?)]
  [app-exp (rator lc-exp?) (rand lc-exp?)])

(define unparse-lc-exp
  (lambda (exp)
    (cases lc-exp exp
      (var-exp (var) var)
      (lambda-exp (bound-var body)
                 (list `lambda (list bound-var)
                      (unparse-lc-exp body)))
      (app-exp (rator rand)
              (list
               (unparse-lc-exp rator) (unparse-lc-exp rand))))))

(define (turn-list-into-string lst)
  (letrec
      ([aux
        (lambda (remains)
          (if (null? remains)
             ""
             (let ([head (car remains)] [rest (cdr remains)])
               ;All elements in the lists here are symbols.
               (string-append
                ((if (symbol? head)
                   symbol->string
                   turn-list-into-string)
                 head)
                   " "
                   (aux rest)))))])
    (string-append "(" (aux lst) ")")))

(define (unparse-to-string exp)
  (let ([tmp (unparse-lc-exp exp)])
    (if (list? tmp)
       (turn-list-into-string tmp)
       (symbol->string tmp))))

(define exp
  (app-exp
    (lambda-exp `a (var-exp `what))
    (var-exp `sadkalj)))