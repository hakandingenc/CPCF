#lang racket

(provide CPCF-I
         CPCF-O
         CPCF-O-Γ)

(require redex)

(define-language CPCF
  [t   o (-> t t) (con t)]
  [o   I B]
  [κ   (flat e) (-> κ κ) (->d κ (λ (x : t) κ))]
  [e   v x (e e) (μ (x : t) e) (op e e) (zero? e) (if e e e) (mon (k l j) κ e)]
  [op op1 op2 op3]
  [op1  + -]
  [op2  and or]
  [op3 * > < =]
  [op* op1 op3]
  [v   c (λ (x : t) e)]
  [c   n boolean]
  [n   integer]
  [(k l j) string]

  [(x y)  variable-not-otherwise-mentioned]

  [E  hole (E e) (v E) (op E e) (op v E) (zero? E)
      (if E e e) (mon (l l l) κ E)]
  #:binding-forms
  (->d κ (λ (x : t) κ #:refers-to x))
  (μ (x : t) e #:refers-to x)
  (λ (x : t) e #:refers-to x))

(define-extended-language CPCF-I
  CPCF
  [e .... (error l l) (check (l l) e v)]
  [E .... (check (l l) E v)])

(define-extended-language CPCF-O
  CPCF-I
  [κ    (flat-ob e (l ...)) (-> κ κ) (->d κ (λ (x : t) κ))]
  [e    .... (own e l)]
  [v    .... (own v l)]
  [E    .... (own E l)]
  #:binding-forms
  (->d κ (λ (x : t) κ #:refers-to x)))

(define-extended-language CPCF-O-Γ
  CPCF-O
  [Γ ∘ (Γ ∪ x : l)])
