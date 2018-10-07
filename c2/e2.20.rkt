#lang eopl
(define (number->tree num)
  (list num (list) (list) (list)))

(define (current-element tree)
  (car tree))

(define (move-to-left tree)
  (if (null? (cadr tree))
      (eopl:error `move-to-left  "Left is empty: ~A.~%" tree)
      (let
          ([current (current-element tree)]
           [left (cadr tree)]
           [right (caddr tree)]
           [ancestors (cadddr tree)])
        (list
         (car left)
         (cadr left)
         (caddr left)
         (cons `left (cons right (cons current ancestors)))))))

(define (move-to-right tree)
  (if
   (null? (caddr tree))
   (eopl:error `move-to-right "Right is empty: ~A.~%" tree)
   (let
          ([current (current-element tree)]
           [left (cadr tree)]
           [right (caddr tree)]
           [ancestors (cadddr tree)])
     (list
      (car right)
      (cadr right)
      (caddr right)
      (cons `right (cons left (cons current ancestors)))))))

(define (insert-to-left num tree)
  (list (car tree) (list num (cadr tree) `()) (caddr tree) (cadddr tree)))

(define (insert-to-right num tree)
  (list (car tree) (cadr tree) (list num `() (caddr tree)) (cadddr tree)))

(define (move-up tree)
  (let
      ([current (current-element tree)]
       [left (cadr tree)]
       [right (caddr tree)]
       [ancestors (cadddr tree)])
    (if (null? ancestors)
        (eopl:error
         `move-up
         "Cannot move up from root: ~A~%" tree)
        (let
            ([direction (car ancestors)]
             [slibling (cadr ancestors)]
             [new-current (caddr ancestors)]
             [higher-ancestors (cdddr ancestors)]
             [current-subtree (list current left right)])
          (if (equal? direction `left)
              (list new-current current-subtree slibling higher-ancestors)
              (list new-current slibling current-subtree higher-ancestors))))))

(define (at-root? tree)
  (null? (cadddr tree)))

(define (at-leaf? tree)
  (and
   (null? (cadr tree))
   (null? (caddr tree))))