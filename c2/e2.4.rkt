#lang eopl
(define (empty-stack) `())

(define (push the-stack x)
  (cons x the-stack))

(define (pop the-stack)
  (if (empty-stack? the-stack)
      (eopl:error `pop "Cannot pop from an empty stack: ~A~%" the-stack)
      (cdr the-stack)))

(define (top the-stack)
  (if (empty-stack? the-stack)
      (eopl:error `top "Cannot fetch top from an empty stack.~%")
      (car the-stack)))

(define empty-stack? null?)