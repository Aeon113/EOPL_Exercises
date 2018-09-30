#lang eopl
(define count-occurrences
  (lambda (s slist)
    (letrec
        ([count
          (lambda (lst num)
            (if (null? lst)
               num
               (let
                   ([head (car lst)] [remain (cdr lst)])
                 (count
                  remain
                  (cond
                    [(equal? head s) (+ 1 num)]
                    [(list? head) (count head num)]
                    [else num])))))])
      (count slist 0))))