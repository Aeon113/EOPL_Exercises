#lang eopl
(define-datatype prefix-exp prefix-exp?
  (const-exp
   (num integer?))
  (diff-exp
   (operand1 prefix-exp?)
   (operand2 prefix-exp?)))

(define-datatype return-val-type return-val-type?
  [return-val (parsed prefix-exp?) (remains list?)])

(define (result->parsed x)
  (cases return-val-type x
    [return-val (parsed remains) parsed]))

(define (result->remains x)
  (cases return-val-type x
    [return-val (parsed remains) remains]))

(define (parse exp-list)
  (letrec
      ([aux
        (lambda (remains)
          (if (null? remains)
             (eopl:error `parse "Too few elements:~%~A~%" remains)
             (let ([head (car remains)] [rest (cdr remains)])
               (cond
                 [(integer? head) (return-val (const-exp head) rest)] ; const-exp
                 [(eqv? `- head) ;diff-exp
                  (letrec
                      ([left-result (aux rest)]
                       [left-parsed (result->parsed left-result)]
                       [left-remains (result->remains left-result)]
                       [right-result (aux left-remains)]
                       [right-parsed (result->parsed right-result)]
                       [right-remains (result->remains right-result)]
                       [final-remains right-remains])
                    (return-val (diff-exp left-parsed right-parsed) final-remains))]
                 [else
                  (eopl:error
                   `parse
                   "Cannot recognize this value:~%~A~%in this expression:~%~A~%"
                   head
                   exp-list)]))))])
    (cases return-val-type (aux exp-list)
      [return-val (parsed remains)
                 (if (null? remains)
                    parsed
                    (eopl:error `parse "Too many elements in this expression:~%~A~%" exp-list))])))