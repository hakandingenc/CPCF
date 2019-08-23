#lang racket

(provide PCF
         source
         CPCF
         CPCF-I
         CPCF-O
         CPCF-O-Γ
         CPCF-O-Δ)

(require redex)

(define-language PCF
  [e        v x (e e) (μ (x : t) e) (op e e) (zero? e) (if e e e)]
  [v        c (λ (x : t) e)]
  [c        n b]
  [n        integer]
  [b        boolean]

  [t        o (-> t t) (con t)]
  [o        I B]

  [op       op1 op2]
  [op1      + -]
  [op2      and or]

  [(x y f)  variable-not-otherwise-mentioned]

  [E        hole (E e) (v E) (op E e) (op v E) (zero? E) (if E e e)]

  #:binding-forms
  (λ (x : t) e #:refers-to x)
  (μ (x : t) e #:refers-to x))

(define-extended-language PCF-κ
  PCF
  [κ       flat-κ ->-κ ->d-κ]
  [flat-κ  (flat e)]
  [->-κ    (-> κ κ)]
  [->d-κ   (->d κ (λ (x : t) κ))]

  #:binding-forms
  (λ (x : t) κ #:refers-to x))

(define-extended-language source
  PCF-κ
  [p  (let* ([x_!_ κ e] ...) e)]

  #:binding-forms
  (let* ([x κ e_x] #:...bind (clauses x (shadow clauses x))) e_body #:refers-to clauses))


(define-extended-language CPCF
  PCF-κ
  [e        .... (mon (l l l) κ e)]

  [(k l j)  string]

  [E        .... (mon (l l l) κ E)])

(define-extended-language CPCF-I
  CPCF
  [e .... (error l l) (check (l l) e v)]

  [E .... (check (l l) E v)])

(define-extended-language CPCF-O
  CPCF-I
  [e       .... (own e l)]
  [v       .... (own v l)]

  [flat-κ  (flat-ob e (l ...))]

  [E       .... (own E l)])

(define-extended-language CPCF-O-Γ
  CPCF-O
  [Γ ∘ (Γ ∪ x : l)])

(define-extended-language CPCF-O-Δ
  CPCF-O
  [Δ ∘ (Δ ∪ x : t)])
