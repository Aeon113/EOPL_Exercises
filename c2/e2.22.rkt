#lang eopl
(define-datatype stack-type stack-type?
  [non-empty-stack-type
   (element (lambda (x) #t))
   (rest stack-type?)]
  [empty-stack-type
   (dummy (lambda (x) #t))])

(define (push the-stack x)
  (non-empty-stack-type x the-stack))

(define (pop the-stack)
  (cases stack-type the-stack
    [non-empty-stack-type
     (element rest)
     rest]
    [empty-stack-type
     (eopl:error
      `pop
      "Cannot pop from an empty stack.~%")]))

(define (top the-stack)
  (cases stack-type the-stack
    [non-empty-stack-type
     (element rest)
     element]
    [empty-stack-type
     (eopl:error
      `top
      "Cannot fetch top from an empty stack.~%")]))

(define (empty-stack) (empty-stack-type `dummy))

(define (empty-stack? x)
  (cases stack-type x
    [empty-stack-type (dummy) #t]
    [else #f]))