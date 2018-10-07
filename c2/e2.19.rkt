#lang eopl
(define (number->bintree num)
  (list num `() `()))

(define (move-to-left tree)
  (cadr tree))

(define (move-to-right tree)
  (caddr tree))

(define (current-element tree)
  (car tree))

(define at-leaf? null?)

(define (insert-to-left num tree)
  (list (car tree)
        (list num (cadr tree) `())
        (caddr tree)))

(define (insert-to-right num tree)
  (list (car tree)
        (cadr tree)
        (list num `() (caddr tree))))