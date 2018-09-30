#lang eopl
(define occurs-free?
  (lambda (target lcExp)
    (cond
      [(not (list? lcExp)) (equal? target lcExp)]
      [(null? lcExp) #t]
      [else (let ([head (car lcExp)] [rest (cdr lcExp)])
         (letrec
             ([contains?
               (lambda (target lst)
                 (if (null? lst)
                    #f
                    (or
                     (equal? target (car lst))
                     (contains? target (cdr lst)))))])
           (if (equal? `lambda head)
              (not (contains? target (car rest)))
              (or
               (occurs-free? target head)
               (occurs-free? target rest)))))])))