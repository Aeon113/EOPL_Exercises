#lang eopl
(define flatten
  (lambda (slist)
    (letrec
        ([aux
          (lambda (remains the-done)
            (cond
              [(not (list? remains)) (cons remains the-done)]
              [(null? remains) the-done]
              [else
               (let ([head (car remains)] [rest-done (aux (cdr remains) the-done)])
                 (if (list? head)
                     (aux head rest-done)
                     (cons head rest-done)))]))])
      (aux slist `()))))