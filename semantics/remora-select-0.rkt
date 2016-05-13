#lang racket

(require rackunit
         redex
         "redex-utils.rkt"
         "language.rkt")
         ;"dependent-lang.rkt")

;; http://stackoverflow.com/a/23680423
(define getNth                     
  (lambda (n list)                   
    (cond ((empty? list) '())             
          ((= n 0) (first list))              
          (else (getNth (- n 1) (rest list))))))

(define-extended-language Remora-Sel0 Arrays
  (expr ....
        (SEL arr natural)))

(define-metafunction Remora-Sel0
  getNthArr : num (elt ...) -> elt
  [(getNthArr num (elt ...)) ,(getNth (term num) (term (elt...)))]
  )

(define Remora-Sel0-Eval
  (extend-reduction-relation
   ->Array
   Remora-Sel0
   [--> (in-hole E (SEL (A (num_0) (elt_0 ...)) num_1))
        (in-hole E ,(getNth (term num_1) (term (elt_0 ...))))
        "select-0"]))

;; TEST

(check-equal?
  (deterministic-reduce
   Remora-Sel0-Eval
   (term (SEL (A (3) (17 1 2)) 0)))
   (term 17))

(check-equal?
  (deterministic-reduce
   Remora-Sel0-Eval
   (term (SEL (A (3) (4 5 6)) 2)))
   (term 6))

