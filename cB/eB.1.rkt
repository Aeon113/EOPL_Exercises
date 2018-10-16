#lang eopl
(define scanner-spec
  '([white-sp (whitespace) skip]
    [num (digit (arbno digit)) number]))

(define grammar-spec
  '([Arith-expr (Arith-term (arbno Additive-op Arith-term)) arith-expr]
    [Arith-term (Arith-factor (arbno Multiplicative-op Arith-factor)) arith-term]
    [Arith-factor (num) const-factor]
    [Arith-factor ("(" Arith-expr ")") expr-factor]
    [Additive-op ("+") plus-op]
    [Additive-op ("-") minus-op]
    [Multiplicative-op ("*") mult-op]
    [Multiplicative-op ("/") div-op]))

(sllgen:make-define-datatypes scanner-spec grammar-spec)
(define scan&parse (sllgen:make-string-parser scanner-spec grammar-spec))

