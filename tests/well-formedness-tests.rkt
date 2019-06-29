#lang racket
(require redex
         rackunit
         "../src/well-formedness.rkt")

(check-true (judgment-holds (well-formed (∘ ∪ x : "l") "l" (own x "l"))))
(check-true (judgment-holds (well-formed ∘ "l" 3)))
(check-true (judgment-holds (well-formed ∘ "l" (3 5))))
(check-true (judgment-holds (well-formed (∘ ∪ x : "l") "l" x)))
(check-false (judgment-holds (well-formed ∘ "l" x)))
