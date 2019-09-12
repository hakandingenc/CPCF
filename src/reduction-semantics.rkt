#lang racket
(provide CPCF-I-red
         CPCF-IO-red)

(require redex
         "languages.rkt"
         "utils.rkt")

(define CPCF-I-red
  (reduction-relation
   CPCF-I
   #:domain e

   ; IS THIS ELABORATION APPROPRIATE?
   ; (i.e. What happens when there's only one rule)
   (-->E (op1 n_1 n_2)
         n_result
         "op1"
         (where n_result (δ (op1 n_1 n_2))))

   (-->E (op2 b_1 b_2)
         b_result
         "op2"
         (where b_result (δ (op2 b_1 b_2))))

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

   (-->E (μ (x : t) e)
         (substitute e x (μ (x : t) e))
         "μ")

   (-->E (mon (k l j) (flat e) c)
         (check (k j) (e c) c)
         "mon-first")

   (-->E (mon (k l j) (-> κ_1 κ_2) v)
         (λ (x : t) (mon (k l j) κ_2 (v (mon (l k j) κ_1 x))))
         "mon-higher"
         (where (λ (_ : t) _) v))

   (-->E (mon (k l j) (->d κ_1 (λ (x : t_1) κ_2)) v)
         (λ (y : t_2) (mon (k l j) (substitute κ_2
                                               x
                                               y)
                           (v (mon (l k j) κ_1 y))))
         "mon-d"
         (fresh y)
         (where (λ (_ : t_2) _) v))

   (-->E (check (k j) #t v)
         v
         "check-t")

   (-->E (check (k j) #f v)
         (error k j)
         "check-f")

   ; Reduction relation for discarding a context with `error`
   (--> (in-hole E (error k j))
        (error k j)
        "error"
        ; Prevent cycle in the trace graph
        (side-condition (not (equal? (term E)
                                     (term hole)))))

   with
   [(--> (in-hole E a1) (in-hole E a2))
    (-->E a1 a2)]))

(define CPCF-IO-red
  (reduction-relation
   CPCF-IO
   #:domain e

   ; IS THIS ELABORATION APPROPRIATE?
   ; (i.e. What happens when there's only one rule)
   (--> (in-hole E (op1 v_1 v_2))
        (in-hole E n_result)
        "op1"
        (where n_1 (get-value v_1))
        (where n_2 (get-value v_2))
        (where l (closest-label E))
        (where n_result (δ (op1 n_1 n_2)))
        (side-condition (term (term-number/label? v_1 l)))
        (side-condition (term (term-number/label? v_2 l))))

   (--> (in-hole E (zero? v))
        (in-hole E #t)
        "zero?-t"
        (where l (closest-label E))
        (side-condition (term (term-match/label? 0 v l))))

   (--> (in-hole E (zero? v))
        (in-hole E #f)
        "zero?-f"
        (where l (closest-label E))
        (side-condition (term (term-number? v)))
        (side-condition (term (term-different/label? 0 v l))))

   (--> (in-hole E (op2 v_1 v_2))
        (in-hole E b_result)
        "op2"
        (where b_1 (get-value v_1))
        (where b_2 (get-value v_2))
        (where l (closest-label E))
        (where b_result (δ (op2 b_1 b_2)))
        (side-condition (term (label-match? v_1 l)))
        (side-condition (term (label-match? v_2 l))))

   (--> (in-hole E (if v e_1 e_2))
        (in-hole E e_1)
        "if-t"
        (where l (closest-label E))
        (side-condition (term (term-match/label? #t v l))))

   (--> (in-hole E (if v e_1 e_2))
        (in-hole E e_2)
        "if-f"
        (where l (closest-label E))
        (side-condition (term (term-match/label? #f v l))))

   (--> (in-hole E (v_1 v_2))
        (in-hole E (own (substitute e x (own (get-value v_2) l)) l))
        "β"
        (where (λ (x : t) e) (get-value v_1))
        (where l (closest-label E))
        (side-condition (term (label-match? v_1 l)))
        (side-condition (term (label-match? v_2 l))))

   (--> (in-hole E (μ (x : t) e))
        (in-hole E (substitute e x (own (μ (x : t) e) l)))
        (where l (closest-label E))
        "μ")

   ; Without other guarantees, this rule changes where the evaluation gets stuck
   ; for a monitor with a function contract and a non-function value
   ; (i.e. This rule only applies if the value is a function which is not the case in the original rule)
   (--> (in-hole E (mon (k l j) (-> κ_1 κ_2) v))
        (in-hole E (λ (x : t) (mon (k l j) κ_2 (v (mon (l k j) κ_1 x)))))
        "mon-higher"
        (fresh x)
        (where (λ (_ : t) _) (get-value v))
        (side-condition (equal? (term l) (term (closest-label E)))))

   (--> (in-hole E (mon (k l j) (flat-ob e (l_ob ...)) v))
        (in-hole E (check (k j) (e c) c))
        "mon-first"
        (where c (get-value v))
        (side-condition (equal? (term l) (term (closest-label E)))))

   (--> (in-hole E (check (k j) v_1 v_2))
        (in-hole E v_2)
        "check-t"
        (side-condition (term (term-match/label? #t v_1 j))))

   (--> (in-hole E (check (k j) v_1 v_2))
        (in-hole E (error k j))
        "check-f"
        (side-condition (term (term-match/label? #f v_1 j))))

   (--> (in-hole E (mon (k l j) (->d κ_1 (λ (x : t_1) κ_2)) v))
        (in-hole E (λ (y : t_2) (mon (k l j)
                                     (substitute/c κ_2
                                                   x
                                                   (mon (l j j) κ_1 y))
                                     (v (mon (l k j) κ_1 y)))))
        "indy"
        (fresh y)
        (where (λ (_ : t_2) _) (get-value v))
        (side-condition (equal? (term l) (term (closest-label E)))))

   ; Reduction relation for discarding a context with `error`
   (--> (in-hole E (error k j))
        (error k j)
        "error"
        ; Prevent cycle in the trace graph
        (side-condition (not (equal? (term E)
                                     (term hole)))))))

#|
(--> (in-hole E (mon (k l j) (->d κ_1 (λ (x : t_1) κ_2)) v))
     (in-hole E (λ (y : t_2) (mon (k l j)
                                  (substitute/c κ_2
                                                x
                                                (mon (l k j) κ_1 y))
                                  (v (mon (l k j) κ_1 y)))))
     "picky"
     (fresh y)
     (where (λ (_ : t_2) _) (get-value v))
     (side-condition (equal? (term l) (term (closest-label E)))))

(--> (in-hole E (mon (k l j) (->d κ_1 (λ (x : t_1) κ_2)) v))
     (in-hole E (λ (y : t_2) (mon (k l j)
                                  (substitute/c κ_2
                                                x
                                                y)
                                  (v (mon (l k j) κ_1 y)))))
     "lax"
     (fresh y)
     (where (λ (_ : t_2) _) (get-value v))
     (side-condition (equal? (term l) (term (closest-label E)))))
|#
