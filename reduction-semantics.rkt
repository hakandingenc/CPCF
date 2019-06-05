#lang racket
(provide δ
         CPCF-red)

(require redex
         "language.rkt"
         "utils.rkt")

(define-metafunction CPCF-O
  [(δ (+ n_1 n_2))   ,(+   (term n_1) (term n_2))]
  [(δ (- n_1 n_2))   ,(-   (term n_1) (term n_2))]
  [(δ (and v_1 v_2)) ,(and (term v_1) (term v_2))]
  [(δ (or v_1 v_2))  ,(or  (term v_1) (term v_2))])

(define CPCF-red
  (reduction-relation
   CPCF-O
   #:domain e

   ; IS THIS ELABORATION APPROPRIATE?
   ; (i.e. What happens when there's only one rule)
   (-->E (op1 n_1 n_2)
         (δ (op1 n_1 n_2))
         "op1")

   (-->E (op2 v_1 v_2)
         (δ (op2 v_1 v_2))
         "op2")

   (-->E (zero? 0)
         #t
         "zero?-t")

   (-->E (zero? n)
         #f
         "zero?-f"
         (side-condition (not (= (term n) 0))))

   (-->E (if #t e_1 e_2)
         e_1
         "if-t")

   (-->E (if #f e_1 e_2)
         e_2
         "if-f")

   (-->E ((λ (x : t) e) v)
         (substitute e x v)
         "β")

   ; TO-DO: μ

   (-->E (mon (k l j) (own-flat e (l_ob ...)) c)
         (check (k j) (e c) c)
         "mon-first")

   (-->E (mon (k l j) (-> κ_1 κ_2) v)
         (λ (x : t) (mon (k l j) κ_2 (v (mon (l k j) κ_1 x))))
         "mon-higher"
         (where/error (λ (y : t) e) v))

   (-->E (check (k j) #t v)
         v
         "check-t")

   (-->E (check (k j) #f v)
         error
         "check-f")

   ; Reduction relation for discarding a context with `error`
   (--> (in-hole E error)
        error
        "error"
        ; Prevent cycle in the trace graph
        (side-condition (not (equal? (term E)
                                     (term hole)))))

   with
   [(--> (in-hole E a1) (in-hole E a2))
    (-->E a1 a2)]))
