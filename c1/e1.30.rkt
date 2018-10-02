#lang eopl
; Implementation 1
; Selection Sort
(define sort/predicate
  (lambda (pred lst)
    (letrec
        ([aux
          (lambda (l)
            (if (eq? 1 (length l))
                l
                (let ([result (aux (cdr l))])
                  (let
                      ([current-head (car l)]
                       [result-head (car result)]
                       [result-rest (cdr result)])
                    (if (pred current-head result-head)
                        (cons current-head result)
                        (cons result-head (cons current-head result-rest)))))))])
      (if (null? lst)
          lst
          (let ([result (aux lst)])
            (cons (car result) (sort/predicate pred (cdr result))))))))

; Implementation 2
; Quick Sort
(define sort/predicate2
  (lambda (pred lst)
    (let
        ([pred-not
          (lambda (lhs rhs) (not (pred lhs rhs)))])
      (letrec
          ([merge-l-to-r
            (lambda (lhs rhs)
              (if (null? lhs)
                  rhs
                  (merge-l-to-r (cdr lhs) (cons (car lhs) rhs))))]
           [aux
            (lambda (sentinel remains left right)
              (if (null? remains)
                  ; Now, all other elements are in left or right
                  (let
                      ([left-result (sort/predicate2 pred-not left)]
                       [right-result (sort/predicate2 pred right)])
                    (merge-l-to-r left-result (cons sentinel right-result)))
                  (let ([target (car remains)] [rest (cdr remains)])
                    (if (pred target sentinel)
                        (aux sentinel rest (cons target left) right)
                        (aux sentinel rest left (cons target right))))))])
        (if (< (length lst) 2)
            lst
            (aux (car lst) (cdr lst) `() `()))))))