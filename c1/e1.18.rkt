#lang racket
(define swaper
  (lambda (s1 s2 slist)
    (let
        ([swap
          (lambda (target)
            (cond
              [(equal? target s1) s2]
              [(equal? target s2) s1]
              [(list? target) (swaper s1 s2 target)]
              [else target]))])
      (if (null? slist)
         slist
         (cons (swap (car slist)) (swaper s1 s2 (cdr slist)))))))