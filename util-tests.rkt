#lang racket

(require redex
         "language.rkt"
         "utils.rkt")

(test-equal (term (closest-label hole)) (term "l0"))
