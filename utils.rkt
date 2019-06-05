#lang racket
(provide closest-label
         initial-label
         unique-owner
         unique-owner/1
         unique-owner/2)

(require redex
         "language.rkt")

(define-metafunction CPCF-O
  [(closest-label-helper hole #f) "l0"]
  [(closest-label-helper hole l) l]
  [(closest-label-helper (E e) l) (closest-label-helper E l)]
  [(closest-label-helper (v E) l) (closest-label-helper E l)]
  [(closest-label-helper (op E e) l) (closest-label-helper E l)]
  [(closest-label-helper (op v E) l) (closest-label-helper E l)]
  [(closest-label-helper (zero? E) l) (closest-label-helper E l)]
  [(closest-label-helper (if E e e) l) (closest-label-helper E l)]
  [(closest-label-helper (mon (l* k j) κ E) l) (closest-label-helper E l*)]
  [(closest-label-helper (check (k l*) E v) l) (closest-label-helper E l*)]
  [(closest-label-helper (own E l*) l) (closest-label-helper E l*)])

(define initial-label (make-parameter "l0"))
(define-metafunction CPCF-O
  [(closest-label E) (closest-label-helper E ,(initial-label))])

(define-metafunction CPCF-O
  [(unique-owner e) e])

(define (unique-owner/1 e [l '()])
  (cond [(and (list? e)
              (equal? (first e) 'own)
              (empty? l))
         (unique-owner/1 (second e) (list (third e)))]
        [(and (list? e)
              (equal? (first e) 'own)
              (not (empty? l)))
         (and (equal? (first l) (third e)) (unique-owner/1 (second e) l))]
        [(and (list? e)
              (not (equal? (first e) 'own)))
         (unless (equal? (first e) 'λ)
           (error 'unique-owner/1 "non-λ input"))
         #t]
        [(not (list? e))
         (unless (or (boolean? e) (integer? e))
           (error 'unique-owner/1 "non-constant input"))
         #t]))

(define (unique-owner/2 e [l '()])
  (match e
    [`(own ,e ,new-l)
     (if (empty? l)
         (unique-owner/2 e (list new-l))
         (and (equal? (first l) new-l) (unique-owner/2 e l)))]
    [(list 'λ _ ...)
     #t]
    [e
     #:when (or (boolean? e) (integer? e))
     #t]))
