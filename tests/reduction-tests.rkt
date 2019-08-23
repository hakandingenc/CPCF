#lang racket

(require redex
         (for-syntax syntax/parse)
         "../src/languages.rkt"
         "../src/reduction-semantics.rkt"
         "../src/utils.rkt")
#;
(define-syntax (test-->>/v stx)
  (syntax-parse stx
    [(_ e1 e2)
     #`(test-->> CPCF-O-red #:equiv '??? e1 e2)]))

#;
(define (test-->>/v e1 e2) (test-->> CPCF-O-red #:equiv (λ (v1 v2) (term (label-match? ,v1 ,v2))) e1 e2))
;(test-->>/v (term 10) (term 10))
#;
(define-syntax (test-->>/v-l))

(default-language CPCF-O)

(test-->> CPCF-O-red
          (term 10)
          (term 10))

(test-->> CPCF-O-red
          (term #t)
          (term #t))

(test-->> CPCF-O-red
          (term (own (+ 10 20) "owner"))
          (term (own 30 "owner")))
(test-->> CPCF-O-red
          (term (+ 10 20))
          (term 30))


(test-->> CPCF-O-red
          (term (- 10 20))
          (term -10))

(test-->> CPCF-O-red
          (term (zero? 0))
          (term #t))
(test-->> CPCF-O-red
          (term (zero? 5))
          (term #f))

(test-->> CPCF-O-red
          (term (and #t #t))
          (term #t))
(test-->> CPCF-O-red
          (term (and #t #f))
          (term #f))
(test-->> CPCF-O-red
          (term (and #f #f))
          (term #f))
(test-->> CPCF-O-red
          (term (or #t #t))
          (term #t))
(test-->> CPCF-O-red
          (term (or #t #f))
          (term #t))
(test-->> CPCF-O-red
          (term (or #f #f))
          (term #f))


(test-->> CPCF-O-red
          (term (own ((λ (x : I) x) 10) "owner"))
          (term (own (own (own 10 "owner") "owner") "owner")))


(test-->> CPCF-O-red
          (term ((λ (x : I) x) (+ 10 20)))
          (term 30))
(test-->> CPCF-O-red
          (term ((λ (x : I) (zero? x)) (+ 10 20)))
          (term #f))


(test-->> CPCF-O-red
          (term (own
                 (mon ("server" "client" "code")
                      (flat-ob (λ (x : I) (zero? x)) ())
                      0)
                 "client"))
          (term (own 0 "client")))


(test-->> CPCF-O-red
          (term (own
                 ((mon ("server" "client" "code")
                       (-> (flat-ob (λ (x : I) (zero? x)) ())
                           (flat-ob (λ (x : I) (zero? x)) ()))
                       (λ (x : (-> I I)) x))
                  0)
                 "client"))
          (term (own (own 0 "client") "client")))


(test-->> CPCF-O-red
          (term (own
                 (((mon ("server" "client" "code")
                        (->
                         (-> (flat-ob (λ (x : I) (zero? x)) ())
                             (flat-ob (λ (x : I) (zero? x)) ()))
                         (-> (flat-ob (λ (x : I) (zero? x)) ())
                             (flat-ob (λ (x : I) (zero? x)) ())))
                        (λ (f : (-> (-> I I) (-> I I))) f))
                   (λ (x : (-> I I)) x))
                  0)
                 "client"))
          (term 0))
