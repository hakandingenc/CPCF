#lang racket

(require redex
         "../src/languages.rkt"
         "../src/translation.rkt"
         "../src/utils.rkt"
         "../src/well-formedness.rkt"
         "../src/reduction-semantics.rkt")

(define arrn apply-reduction-relation*/n)

(define my-n 0)

(redex-check
 source
 p
 #:ad-hoc
 (let ([i 50])
   (set! my-n (add1 my-n))
   (when (zero? (modulo my-n 200)) (println my-n))
   (define fl (apply-reduction-relation*/n CPCF-I-red (term (decorate "b" p)) i))
   (define sl (apply-reduction-relation*/n CPCF-IO-red (term (decorate "b/o" p)) i))
   (when (not (= (length fl) (length sl)))
     (traces CPCF-I-red (term (decorate "b" p)))
     (traces CPCF-IO-red (term (decorate "b/o" p)))
     (error))
   (andmap
    (λ (t1 t2)
      (unless (alpha-equivalent? CPCF-I t1 (term (undecorate ,t2) #:lang CPCF-IO))
        (printf "Returning #f because ~v and ~v are not α-eq where t1 = ~v~n" (term (undecorate ,t1) #:lang CPCF-IO) t2 t1)
        #f))
    fl
    sl))
 #:attempts 50000
 #:keep-going? #t)

#|
(for/and ([O-terms (in-list (map (λ (t) (term undecorate t)) (apply-reduction-relation*/n CPCF-IO-red (term e) i)))]
               [I-terms (in-list (apply-reduction-relation*/n CPCF-I-red (term (undecorate e)) i))])
       (unless (alpha-equivalent? CPCF-I (term (undecorate ,t1) #:lang CPCF-IO) t2)
         #;(printf "Returning #f because ~v and ~v are not α-eq where t1 = ~v~n" (term (undecorate ,t1) #:lang CPCF-IO) t2 t1)
         #f))
|#
