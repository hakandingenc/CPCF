#lang racket
(provide δ
         closest-label
         unowned-label
         term-number/label?
         term-match/label?
         label-match?
         term-different/label?
         get-value
         substitute/c
         contains?
         core-term
         types)

(require redex
         "languages.rkt")

(define-metafunction CPCF-O
  [(δ (+ n_1 n_2))   ,(+   (term n_1) (term n_2))]
  [(δ (- n_1 n_2))   ,(-   (term n_1) (term n_2))]
  [(δ (and v_1 v_2)) ,(and (term v_1) (term v_2))]
  [(δ (or v_1 v_2))  ,(or  (term v_1) (term v_2))]
  [(δ (* n_1 n_2))   ,(*   (term n_1) (term n_2))]
  [(δ (< n_1 n_2))   ,(<   (term n_1) (term n_2))]
  [(δ (> n_1 n_2))   ,(>   (term n_1) (term n_2))]
  [(δ (= n_1 n_2))   ,(=   (term n_1) (term n_2))])

(define-metafunction CPCF-O
  [(closest-label-helper hole l) l]
  [(closest-label-helper (E e) l) (closest-label-helper E l)]
  [(closest-label-helper (v E) l) (closest-label-helper E l)]
  [(closest-label-helper (op E e) l) (closest-label-helper E l)]
  [(closest-label-helper (op v E) l) (closest-label-helper E l)]
  [(closest-label-helper (zero? E) l) (closest-label-helper E l)]
  [(closest-label-helper (if E e_1 e_2) l) (closest-label-helper E l)]
  [(closest-label-helper (mon (l_2 k j) κ E) l_1) (closest-label-helper E l_2)]
  [(closest-label-helper (check (k l_2) E v) l_1) (closest-label-helper E l_2)]
  [(closest-label-helper (own E l_2) l_1) (closest-label-helper E l_2)])

(define unowned-label (make-parameter "unowned"))
(define-metafunction CPCF-O
  [(closest-label E) (closest-label-helper E ,(unowned-label))])

(define-metafunction CPCF-O
  [(term-number/label? (own v l_2) l_1)
   ,(and (equal? (term l_1) (term l_2))
         (term (term-number/label? v l_2)))]
  [(term-number/label? n l) #t]
  [(term-number/label? v l) #f])

(define-metafunction CPCF-O
  [(term-match/label? v_1 (own v_2 l_2) l_1)
   ,(and (equal? (term l_1) (term l_2))
         (term (term-match/label? v_2 v_1 l_1)))]
  [(term-match/label? v_1 v_2 l)
   ,((default-equiv) (term v_2) (term v_1))])

(define-metafunction CPCF-O
  [(label-match? (own v l_2) l_1)
   ,(and (equal? (term l_1) (term l_2))
         (term (label-match? v l_1)))]
  [(label-match? v l)
   #t])

(define-metafunction CPCF-O
  [(term-different/label? v_1 (own v_2 l_2) l_1)
   ,(and (equal? (term l_1) (term l_2))
         (term (term-different/label? v_2 v_1 l_1)))]
  [(term-different/label? v_1 v_2 l_1)
   ,(not ((default-equiv) (term v_2) (term v_1)))])

(define-metafunction CPCF-O
  [(get-value (own v l)) (get-value v)]
  [(get-value v) v])

(define-metafunction CPCF-O
  substitute/c : κ x e -> κ
  [(substitute/c (flat-ob (own e_2 l_2) (l_ob ...)) x e_1)
   (flat-ob (substitute (own e_2 l_2) x (own e_1 l_2)) (l_ob ...))]
  [(substitute/c (-> κ_1 κ_2) x e)
   (-> (substitute/c κ_1 x e) (substitute/c κ_2 x e))]
  [(substitute/c (->d κ_1 (λ (x_1 : t) κ_2)) x_1 e)
   (->d (substitute/c κ_1 x_1 e) (λ (x_1 : t) κ_2))]
  [(substitute/c (->d κ_1 (λ (y : t) κ_2)) x e)
   (->d (substitute/c κ_1 x e) (λ (y : t) (substitute/c κ_2 x e)))])

(define-metafunction CPCF-O-Γ
  contains? : Γ x l -> boolean
  [(contains? ∘ x l) #f]
  [(contains? (Γ ∪ x : l_1) y l_2)
   ,(or
     (and (equal? (term x) (term y))
          (equal? (term l_1) (term l_2)))
     (term (contains? Γ y l_2)))])

(define-judgment-form CPCF-O-Γ
  #:mode (core-term I)
  #:contract (core-term e)

  [-------------------------
   (core-term c)]

  [(core-term e)
   -------------------------
   (core-term (λ (x : t) e))]

  [-------------------------
   (core-term x)]

  [(core-term e_1)
   (core-term e_2)
   -------------------------
   (core-term (e_1 e_2))]

  [(core-term e)
   -------------------------
   (core-term (μ (x : t) e))]

  [(core-term e_1)
   (core-term e_2)
   -------------------------
   (core-term (op e_1 e_2))]

  [(core-term e)
   -------------------------
   (core-term (zero? e))]

  [(core-term e_1)
   (core-term e_2)
   (core-term e_3)
   -------------------------
   (core-term (if e_1 e_2 e_3))]

  [(core-term e)
   -------------------------
   (core-term (mon (k l j) κ e))])

(define-judgment-form CPCF-O-Γ-Δ
  #:mode (types I I O)
  #:contract (types Δ all t)

  [------------------------- "I"
   (types Δ number I)]

  [------------------------- "B"
   (types Δ boolean B)]

  [(types (Δ ∪ x : t_1) e t_2)
   ------------------------- "λ"
   (types Δ (λ (x : t_1) e) (-> t_1 t_2))]

  [--------------------- "var"
   (types (Δ ∪ x : t) x t)]

  [(types Δ x_1 t_1)
   (side-condition (different x_1 x_2))
   ------------------------------------ "var-extend"
   (types (Δ ∪ x_2 : t_2) x_1 t_1)]

  [(types Δ e_1 (-> t_1 t_2))
   (types Δ e_2 t_1)
   ------------------------- "app"
   (types Δ (e_1 e_2) t_2)]

  [(types (Δ ∪ x : t_1) e t_1)
   ------------------------- "μ"
   (types Δ (μ (x : t_1) e) t_1)]

  [(types Δ e_1 B)
   (types Δ e_2 t_1)
   (types Δ e_3 t_1)
   -----------------------------"if"
   (types Δ (if e_1 e_2 e_3) t_1)]

  [(types Δ e I)
   -----------------------------"zero?"
   (types Δ (zero? e) B)]

  [(types Δ e_1 I)
   (types Δ e_2 I)
   -----------------------------"op1,3"
   (types Δ (op* e_1 e_2) I)]

  [(types Δ e_1 B)
   (types Δ e_2 B)
   -----------------------------"op2"
   (types Δ (op2 e_1 e_2) B)]

  [(types Δ e t)
   (types Δ κ (con t))
   -----------------------------"mon"
   (types Δ (mon (k l j) κ e) t)])
