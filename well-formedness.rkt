#lang racket
(provide well-formed
         well-formed/c
         loosely-well-formed)

(require redex
         "languages.rkt"
         "utils.rkt")

(define-judgment-form
  CPCF-O-Γ
  #:mode     (well-formed I I I)
  #:contract (well-formed Γ l e)

  [------------------------- "c"
   (well-formed Γ l c)]

  [(well-formed Γ l e_1)
   (well-formed Γ l e_2)
   ------------------------- "app"
   (well-formed Γ l (e_1 e_2))]

  [(well-formed Γ l e_1)
   (well-formed Γ l e_2)
   (well-formed Γ l e_3)
   ------------------------- "if"
   (well-formed Γ l (if e_1 e_2 e_3))]

  [(well-formed Γ l e)
   ------------------------- "zero?"
   (well-formed Γ l (zero? e))]

  [(well-formed Γ l e_1)
   (well-formed Γ l e_2)
   ------------------------- "op"
   (well-formed Γ l (op e_1 e_2))]

  [(well-formed (Γ ∪ x : l) l e)
   ------------------------- "λ"
   (well-formed Γ l (λ (x : t) e))]

  [(well-formed (Γ ∪ x : l) l e)
   ------------------------- "μ"
   (well-formed Γ l (μ (x : t) e))]

  [(well-formed Γ l e)
   ------------------------- "own"
   (well-formed Γ l (own e l))]

  [(side-condition (contains? Γ x l))
   ------------------------- "var"
   (well-formed Γ l x)]

  [------------------------- "error"
   (well-formed Γ l (error k j))]

  [(loosely-well-formed Γ j e)
   (well-formed Γ l v)
   ------------------------- "check"
   (well-formed Γ l (check (k j) e v))]

  [(loosely-well-formed Γ k e)
   (well-formed/c Γ (k) (l) j κ)
   ------------------------- "mon"
   (well-formed Γ l (mon (k l j) κ e))])

(define-judgment-form
  CPCF-O-Γ
  #:mode     (well-formed/c I I       I       I I)
  #:contract (well-formed/c Γ (l ...) (l ...) l κ)

  [(well-formed Γ j e)
   (side-condition ,(subset? (list->set (term (k_1 ...)))
                             (list->set (term (k_2 ...)))))
   ------------------------- "flat-ob"
   (well-formed/c Γ (k_1 ...) (l ...) j (flat-ob (own e j) (k_2 ...)))]

  [(well-formed/c Γ (l ...) (k ...) j κ_1)
   (well-formed/c Γ (k ...) (l ...) j κ_2)
   ------------------------- "->"
   (well-formed/c Γ (k ...) (l ...) j (-> κ_1 κ_2))]

  [(well-formed/c Γ (l ...) (k ... j) j κ_1)
   (well-formed/c (Γ ∪ x : j) (k ...) (l ...) j κ_2)
   ------------------------- "->d"
   (well-formed/c Γ (k ...) (l ...) j (->d κ_1 (λ (x : t) κ_2)))])

(define-judgment-form
  CPCF-O-Γ
  #:mode     (loosely-well-formed I I I)
  #:contract (loosely-well-formed Γ l e)

  [(well-formed Γ l e)
   ------------------------- "own"
   (loosely-well-formed Γ l (own e l))]

  [(side-condition (contains? Γ x l))
   ------------------------- "var"
   (loosely-well-formed Γ l x)]

  [(loosely-well-formed Γ l e_1)
   (loosely-well-formed Γ l e_2)
   ------------------------- "app"
   (loosely-well-formed Γ l ((own e_1 l ) e_2))])

#|
Overwritten Rules

[(well-formed Γ k e)
 (well-formed/c Γ (k) (l) j κ)
 (side-condition ,(not (equal? (term k) (term l))))
 ------------------------- "mon"
 (well-formed Γ l (mon (k l j) κ (own e k)))]

[(well-formed Γ j e)
 ------------------------- "flat-ob"
 (well-formed/c Γ (k ...) (l ...) j (flat-ob (own e j) (k ...)))]
|#
