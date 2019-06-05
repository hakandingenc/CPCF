#lang racket

(require redex
         "language.rkt"
         "reduction-semantics.rkt")

(test-->> CPCF-red
          (term 10)
          (term 10))

(test-->> CPCF-red
          (term #t)
          (term #t))

(test-->> CPCF-red
          (term (+ 10 20))
          (term 30))
(test-->> CPCF-red
          (term (- 10 20))
          (term -10))

(test-->> CPCF-red
          (term (zero? 0))
          (term #t))
(test-->> CPCF-red
          (term (zero? 5))
          (term #f))

(test-->> CPCF-red
          (term (and #t #t))
          (term #t))
(test-->> CPCF-red
          (term (and #t #f))
          (term #f))
(test-->> CPCF-red
          (term (and #f #f))
          (term #f))
(test-->> CPCF-red
          (term (or #t #t))
          (term #t))
(test-->> CPCF-red
          (term (or #t #f))
          (term #t))
(test-->> CPCF-red
          (term (or #f #f))
          (term #f))

(test-->> CPCF-red
          (term ((λ (x : I) x) 10))
          (term 10))
(test-->> CPCF-red
          (term ((λ (x : I) x) (+ 10 20)))
          (term 30))
(test-->> CPCF-red
          (term ((λ (x : I) (zero? x)) (+ 10 20)))
          (term #f))

(test-->> CPCF-red
          (term (mon ("server" "client" "code")
                     (own-flat (λ (x : I) (zero? x)) ())
                     0))
          (term 0))

(test-->> CPCF-red
          (term (((mon ("server" "client" "code")
                       (->
                        (-> (own-flat (λ (x : I) (zero? x)) ())
                            (own-flat (λ (x : I) (zero? x)) ()))
                        (-> (own-flat (λ (x : I) (zero? x)) ())
                            (own-flat (λ (x : I) (zero? x)) ())))
                       (λ (f : (-> (-> I I) (-> I I))) f))
                  (λ (x : (-> I I)) x))
                 0))
          (term 0))
