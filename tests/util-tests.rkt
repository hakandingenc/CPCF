#lang racket

(require redex
         "../src/languages.rkt"
         "../src/utils.rkt")

(default-language CPCF-O)

(test-equal (term (closest-label hole)) (unowned-label))

(test-equal (term (term-match/label? 0 0 "label")) (term #t))

(test-equal (term (get-value 10)) (term 10))
(test-equal (term (get-value (own 10 "label"))) (term 10))

(test-equal (term (substitute/c (flat-ob (own x "owner") ("ob1" "ob2")) x 10))
            (term (flat-ob (own (own 10 "owner") "owner") ("ob1" "ob2"))))

(test-equal (term (substitute/c
                   (->d
                    (flat-ob (own (λ (x : I) (zero? x))
                                   "owner")
                              ())
                    (λ (x : I)
                      (flat-ob (λ (x : I) (zero? x))
                                ())))
                   x
                   10))
            (term (->d
                   (flat-ob (own (λ (x : I) (zero? x))
                                  "owner")
                             ())
                   (λ (x : I)
                     (flat-ob (λ (x : I) (zero? x))
                               ())))))

(test-equal (term (substitute/c
                   (->d
                    (flat-ob (own (λ (y : I) (zero? x))
                                   "owner")
                              ())
                    (λ (x : I)
                      (flat-ob (λ (x : I) (zero? x))
                                ())))
                   x
                   10))
            (term (->d
                   (flat-ob (own (λ (y : I) (zero? (own 10 "owner")))
                                  "owner")
                             ())
                   (λ (x : I)
                     (flat-ob (λ (x : I) (zero? x))
                               ())))))

(test-equal (term (substitute/c
                   (->d
                    (flat-ob (own (λ (x : I) (zero? x))
                                   "owner")
                              ())
                    (λ (y : I)
                      (flat-ob (own (λ (y : I) (zero? x)) "owner")
                                ())))
                   x
                   10))
            (term (->d
                   (flat-ob (own (λ (x : I) (zero? x))
                                  "owner")
                             ())
                   (λ (y : I)
                     (flat-ob (own (λ (y : I) (zero? (own 10 "owner")))
                                    "owner")
                               ())))))
