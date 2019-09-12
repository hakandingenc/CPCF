#lang racket

(require redex
         "../src/languages.rkt"
         "../src/reduction-semantics.rkt"
         "../src/well-formedness.rkt"
         "../src/typing-judgment.rkt"
         "../src/utils.rkt"
         "../src/translation.rkt")

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

(define match-value (redex-match? CPCF-IO v))

; This formulation of the theorem is technically stronger
; (i.e. has stronger assumptions) because it checks for nondeterministic reduction
(define n 0)
#;
(redex-check
 CPCF-O-Γ
 e
 (if (judgment-holds (suitable e))
     (let ([reduction
            (apply-reduction-relation*
             CPCF-IO-red
             (term e))])
       (or
        (and (= (length reduction) 1)
             (let ([irreducible (first reduction)])
               (or
                ;; reduces to a value
                (match-value irreducible)
                ;; diverges (note: not explicitly checking ∀e)
                (= (length (apply-reduction-relation CPCF-IO-red irreducible)) 1))))
        ;; correctly errors
        #;
        (let ([all-reductions (apply-reduction-relation*
                               CPCF-IO-red
                               (term e)
                               #:all? #t)])
          (and (redex-match CPCF-IO

                            (last all-reductions))
               ; TO-DO: Check if there's at least one such thing
               (map (redex-match
                     CPCF-IO
                     (in-hole E
                              (mon (k l-mon j)
                                   (flat-ob e (l ...))
                                   v)))
                    all-reductions))
          ))
       (println (term e)))
     (set! n (add1 n)))
 #:print? #t
 #:attempts 1000)
#;n
#;
(redex-check
 CPCF-O-Γ
 e
 #:ad-hoc
 (when (and
        (judgment-holds (suitable (term e)))
        (member 'mon (flatten (term e))))
   (println (term e)))
 #:attempts 1000000
 #:keep-going? #t)

(define-extended-language source*
  source
  [constants ∘ ∪ :])

(redex-check
 source*
 p
 (let ([my-term (term (decorate "b/o" p))])
   (when (and
          (judgment-holds (suitable ,my-term))
          (println (term p))
          #;(member 'mon (flatten my-term)))
     (println my-term)))
 #:attempts 50000
 #:keep-going? #t)
