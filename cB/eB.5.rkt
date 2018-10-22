#lang eopl
(define scanner-spec
  '([white-sp (whitespace) skip]
    [num (digit (arbno digit)) number]))

(define grammar-spec
  '([Arith-expr (Arith-term (arbno Additive-op Arith-term)) arith-expr]
    [Arith-term (Arith-factor (arbno Multiplicative-op Arith-factor)) arith-term]
    [Arith-factor (Arith-num) const-factor]
    [Arith-factor ("(" Arith-expr ")") expr-factor]
    [Arith-num (num) plain-num]
    [Arith-num ("+" num) pos-num]
    [Arith-num ("-" num) neg-num]
    [Additive-op ("+") plus-op]
    [Additive-op ("-") minus-op]
    [Multiplicative-op ("*") mult-op]
    [Multiplicative-op ("/") div-op]))

(sllgen:make-define-datatypes scanner-spec grammar-spec)
(define scan&parse (sllgen:make-string-parser scanner-spec grammar-spec))

(define (additive-op-parser x)
  (cases Additive-op x
    [plus-op () +]
    [minus-op () -]))

(define (multiplicative-op-parser x)
  (cases Multiplicative-op x
    [mult-op () *]
    [div-op () /]))

(define (expr-evaluator x)
  (letrec
      ([aux
        (lambda (current-val op-list rhs-list)
          (if (null? op-list)
              current-val
              (aux
               ((additive-op-parser (car op-list))
                current-val
                (term-evaluator (car rhs-list)))
               (cdr op-list)
               (cdr rhs-list))))])
    (cases Arith-expr x
      [arith-expr (term op-list rhs-list)
                  (aux (term-evaluator term) op-list rhs-list)])))

(define (term-evaluator x)
  (letrec
      ([aux
        (lambda (current-val op-list rhs-list)
          (if (null? op-list)
              current-val
              (aux
               ((multiplicative-op-parser (car op-list))
                current-val
                (factor-evaluator (car rhs-list)))
               (cdr op-list)
               (cdr rhs-list))))])
    (cases Arith-term x
      [arith-term (factor op-list rhs-list)
                  (aux (factor-evaluator factor) op-list rhs-list)])))

(define (factor-evaluator x)
  (cases Arith-factor x
    [const-factor (num) (num-evaluator num)]
    [expr-factor (expr) (expr-evaluator expr)]))

(define (num-evaluator x)
  (cases Arith-num x
    [plain-num (num) num]
    [pos-num (num) num]
    [neg-num (num) (- 0 num)]))

(define evaluate expr-evaluator)

(define (get-val prog)
  (evaluate (scan&parse prog)))