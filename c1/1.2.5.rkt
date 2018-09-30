#lang eopl
(define subst
  (lambda (new old var)
    (let ([subst-symbol (lambda (smb) (if (equal? old smb) new smb))])
      (letrec
          ([subst-slist
            (lambda (slist)
              (if (null? slist)
                 slist
                 (let ([head (car slist)] [rest (cdr slist)])
                   (cons
                    ((if (symbol? head) subst-symbol subst-slist) head)
                    (subst-slist rest)))))])
        ((if (symbol? var) subst-symbol subst-slist) var)))))