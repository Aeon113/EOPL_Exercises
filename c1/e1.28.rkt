#lang eopl
; Implementation 1
(define merge
  (lambda (loi1 loi2)
    (cond
      [(null? loi1) loi2]
      [(null? loi2) loi1]
      [else
       (let ([l-head (car loi1)]
             [l-rest (cdr loi1)]
             [r-head (car loi2)]
             [r-rest (cdr loi2)])
         (if (< l-head r-head)
             (cons l-head (merge l-rest loi2))
             (cons r-head (merge loi1 r-rest))))])))


; Implmentation 2
(define merge2
  (lambda (loi1 loi2)
    (letrec
        ([insert-2-to-1
          (lambda (master slave)
            (if (null? slave)
                master
                (insert-2-to-1 (cons (car slave) master) (cdr slave))))]
         [aux
          (lambda (left right result)
            (cond
              [(null? left) (insert-2-to-1 result right)]
              [(null? right) (insert-2-to-1 result left)]
              [else
               (let
                   ([l-head (car left)]
                    [l-rest (cdr left)]
                    [r-head (car right)]
                    [r-rest (cdr right)])
                 (if (< l-head r-head)
                     (aux l-rest right (cons l-head result))
                     (aux left r-rest (cons r-head result))))]))])
      (reverse (aux loi1 loi2 `())))))