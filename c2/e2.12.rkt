#lang eopl
(define empty-stack-impl
  (lambda (operation)
    (cond
      [(equal? operation `pop)
       (eopl:error `pop "Cannot pop from an empty stack.~%")]
      [(equal? operation `top)
       (eopl:error `top "Cannot get top from an empty stack.~%")]
      [else
       (eopl:error `empty-stack "No such operation: ~A~%." operation)])))

(define (empty-stack) empty-stack-impl)

(define (empty-stack? the-stack) (equal? empty-stack-impl the-stack))

(define (push the-stack x)
  (lambda (operation)
    (cond
      [(equal? operation `pop)
        the-stack]
       [(equal? operation `top)
        x]
       [else eopl:error
             `stack "No such operation: ~A~%."
             operation])))

(define (top the-stack)
  (the-stack `top))

(define (pop the-stack)
  (the-stack `pop))