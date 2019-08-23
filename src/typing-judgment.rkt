#lang racket
(provide types
         types/c)

(require redex
         "languages.rkt"
         "utils.rkt")

(define-judgment-form CPCF-O-Δ
  #:mode (types I I O)
  #:contract (types Δ e t)

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
   -----------------------------"op1"
   (types Δ (op e_1 e_2) I)]

  [(types Δ e_1 B)
   (types Δ e_2 B)
   -----------------------------"op2"
   (types Δ (op2 e_1 e_2) B)]

  [(types Δ e t)
   (types/c Δ κ (con t))
   -----------------------------"mon"
   (types Δ (mon (k l j) κ e) t)]

  [(types Δ e t)
   -----------------------------"own"
   (types Δ (own e l) t)])

(define-judgment-form CPCF-O-Δ
  #:mode     (types/c I I O)
  #:contract (types/c Δ κ t)

  [(types Δ e (-> t B))
   -----------------------------"flat-ob"
   (types/c Δ (flat-ob e (l ...)) (con t))]

  [(types/c Δ κ_1 (con t_1))
   (types/c Δ κ_2 (con t_2))
   ------------------------- "->"
   (types/c Δ (-> κ_1 κ_2) (con (-> t_1 t_2)))]

  [(types/c Δ κ_1 (con t_1))
   (types/c Δ κ_2 (con t_2))
   ------------------------- "->d"
   (types/c Δ (->d κ_1 (λ (x : t) κ_2)) (con (-> t_1 t_2)))])
