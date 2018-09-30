#lang eopl
(define remove-first
  (lambda (smb lst)
    (if (null? lst)
       lst
       (let
           ([target (car lst)]
            [rest (cdr lst)])
         (if (equal? target smb)
            rest
            (cons target (remove-first smb rest)))))))