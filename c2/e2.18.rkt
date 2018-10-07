#lang eopl
(define (number->sequence num)
  (list num (list) (list)))

(define (current-element seq)
  (car seq))

(define (move-to-left seq)
  (if (null? (cadr seq))
      (eopl:error `move-to-left "Left is empty: ~A.~%" seq)
      (list (car (cadr seq)) (cdr (cadr seq)) (cons (car seq) (caddr seq)))))

(define (move-to-right seq)
  (if (null? (caddr seq))
      (eopl:error `move-to-right "Right is empty: ~A.~%" seq)
      (list (car (caddr seq)) (cons (car seq) (cadr seq)) (cdr (caddr seq)))))

(define (insert-to-left num seq)
  (list (car seq) (cons num (cadr seq)) (caddr seq)))

(define (insert-to-right num seq)
  (list (car seq) (cadr seq) (cons num (caddr seq))))