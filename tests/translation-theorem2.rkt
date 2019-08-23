#lang racket

(require redex
         "../src/languages.rkt"
         "../src/translation.rkt"
         "../src/utils.rkt"
         "../src/well-formedness.rkt"
         "../src/reduction-semantics.rkt")

(define arrn apply-reduction-relation*/n)

(define my-n 0)

(define b-count 0)
(define n-count 0)
(define e-count 0)
(define r-count 0)

(define-metafunction CPCF-O-Γ
  [(contains-error? (error l_1 l_2)) #t]
  [(contains-error? c) #f]
  [(contains-error? (λ (x : t) e)) (contains-error? e)]
  [(contains-error? x) #f]
  [(contains-error? (μ (x : t) e)) (contains-error? e)]
  [(contains-error? (zero? e)) (contains-error? e)]
  [(contains-error? (mon (l_1 l_2 l_3) κ e)) (contains-error? e)]
  [(contains-error? (check (l_1 l_2) e v)) (contains-error? e)]
  [(contains-error? (own e l)) (contains-error? e)]
  [(contains-error? (e_1 e_2)) ,(or (term (contains-error? e_1)) (term (contains-error? e_2)))]
  [(contains-error? (op e_1 e_2)) ,(or (term (contains-error? e_1)) (term (contains-error? e_2)))]
  [(contains-error? (if e_1 e_2 e_3)) ,(or (term (contains-error? e_1)) (term (contains-error? e_2)) (term (contains-error? e_3)))])


(redex-check
 CPCF-O-Γ
 e
 #:ad-hoc
 (begin
   (set! my-n (add1 my-n))
   (when (zero? (modulo my-n 1000)) (println my-n))

   (when (judgment-holds (well-formed ∘ ,(unowned-label) e))

   (cond
     [(redex-match CPCF-O-Γ n (term e)) (set! n-count (add1 n-count))]
     [(redex-match CPCF-O-Γ b (term e)) (set! b-count (add1 b-count))]
     [(term (contains-error? e))        (set! e-count (add1 e-count))]
     [else                              (set! r-count (add1 r-count))])



   #;(println my-n)
   #;(println (term e))
   (let ([i 50])
     (define fl (apply-reduction-relation*/n CPCF-O-red (term e) i))
     (define sl (apply-reduction-relation*/n CPCF-I-red (term (undecorate e)) i))
     (if (= (length fl) (length sl))
         (andmap
          (λ (t1 t2)
            (unless (alpha-equivalent? CPCF-I (term (undecorate ,t1) #:lang CPCF-O) t2)
              (printf "Returning #f because ~v and ~v are not α-eq where t1 = ~v~n" (term (undecorate ,t1) #:lang CPCF-O) t2 t1)
              #f))
          fl
          sl)
         (begin
           (println fl)
           (println sl)
           (traces CPCF-O-red (term e))
           (traces CPCF-I-red (term (undecorate e)))
           (unless
               (and (= (- (length fl) (length sl)) 1)
                    (andmap
                     (λ (t1 t2)
                       (unless (alpha-equivalent? CPCF-I (term (undecorate ,t1) #:lang CPCF-O) t2)
                         (printf "Returning #f because ~v and ~v are not α-eq where t1 = ~v~n" (term (undecorate ,t1) #:lang CPCF-O) t2 t1)
                         #f))
                     (rest fl)
                     sl))
             (error 'downward)))))))
 #:attempts 50000)
(printf "n-count = ~a~nb-count = ~a~ne-count = ~a~nr-count = ~a~n" n-count b-count e-count r-count)
