#lang eopl
(define base 16)

(define (zero) `(0))

(define (is-zero? x)
  (and (equal? 1 (length x)) (equal? 0 (car x))))

(define (successor x)
  (letrec
      ([inc
        (lambda (one-bit)
          (let ([val (+ 1 (car one-bit))])
            (if (equal? val base)
                (list `(0) #t)
                (list (list val) #f))))]
       [aux
        (lambda (remains)
          (if (equal? 1 (length remains))
              (inc remains)
              (letrec
                  ([d (car remains)]
                   [result (aux (cdr remains))]
                   [result-val (car result)]
                   [semi-val (if (cadr result) (+ 1 d) d)]
                   [final-val
                    (cons
                     (if (equal? semi-val base) 0 semi-val)
                     result-val)]
                   [need-inc
                    (equal? semi-val base)])
                (list final-val need-inc))))])
    (let ([result (aux x)])
      (letrec
          ([val (car result)] [need-inc (cadr result)])
        (if need-inc
            (cons 1 val)
            val)))))

(define (predecessor x)
  (letrec
      ([dec (lambda (v) (- (if (equal? 0 v) base v) 1))]
       [get-rid-of-0
        (lambda (v)
          (cond
            [(equal? (length v) 1) v]
            [(equal? (car v) 0) (get-rid-of-0 (cdr v))]
            [else v]))]
       [aux
        (lambda (remains)
          (if (equal? 1 (length remains))
              (list (dec (car remains)))
              (letrec
                  ([head (car remains)]
                   [rest-result (aux (cdr remains))]
                   [rest-result-head (car rest-result)]
                   [need-dec (equal? (- base 1) rest-result-head)])
                (cons (if need-dec (dec head) head) rest-result))))])
    (get-rid-of-0 (aux x))))