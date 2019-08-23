#lang racket

(require redex
         "../src/languages.rkt"
         "../src/reduction-semantics.rkt"
         "../src/well-formedness.rkt"
         "../src/utils.rkt")

(define-union-language CPCF-O-Γ-Δ
  CPCF-O-Γ CPCF-O-Δ)

(define-judgment-form CPCF-O-Γ-Δ
  #:mode (suitable I)
  #:contract (suitable e)

  [(types ∘ e t)
   (well-formed ∘ ,(unowned-label) e)
   -----------------------------
   (suitable e)])

(define (typecheck e)
  (judgment-holds (types ∘ e t) t))

(define match-value (redex-match? CPCF-O v))

; This formulation of the theorem is technically stronger
; (i.e. has stronger assumptions) because it checks for nondeterministic reduction
(define n 0)
#;
(redex-check
 CPCF-O-Γ-Δ-core
 core-e
 #:in-order
 (if (judgment-holds (suitable core-e))
     (let ([reduction
            (apply-reduction-relation*
             CPCF-O-red
             (term core-e))])
       (or
        (and (= (length reduction) 1)
             (let ([irreducible (first reduction)])
               (or
                ;; reduces to a value
                (match-value irreducible)
                ;; diverges (note: not explicitly checking ∀e)
                (= (length (apply-reduction-relation CPCF-O-red irreducible)) 1))))
        ;; correctly errors
        #;
        (let ([all-reductions (apply-reduction-relation*
                               CPCF-O-red
                               (term core-e)
                               #:all? #t)])
          (and (redex-match CPCF-O

                            (last all-reductions))
               ; TO-DO: Check if there's at least one such thing
               (map (redex-match
                     CPCF-O
                     (in-hole E
                              (mon (k l-mon j)
                                   (flat-ob e (l ...))
                                   v)))
                    all-reductions))
          ))
       (println (term core-e)))
     (set! n (add1 n)))
 #:print? #t
 #:attempts 1000)
n

(redex-check
 CPCF-O-Γ-Δ-core
 core-e
 #:ad-hoc
 (if
  (and
   (judgment-holds (suitable (term core-e)))
   (member 'mon (flatten (term core-e))))
  (println (term core-e))
  #t)
  #:attempts 10000000)
