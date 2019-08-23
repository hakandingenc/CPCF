#lang racket
(provide CPCF-mix*
         decorate
         guard-free
         undecorate)

(require redex
         "languages.rkt"
         "well-formedness.rkt"
         "utils.rkt")

(define-union-language CPCF-mix source CPCF-O)
(define-extended-language CPCF-mix*
  CPCF-mix
  [e/κ e κ])

(define-metafunction CPCF-mix*
  decorate : _ p -> e_
  #:post ,(cond
            [(redex-match? CPCF-mix* b/o (term F_))
             (and (redex-match? CPCF-O e (term e_))
                  (when (empty? (term (free-vars e_)))
                    (judgment-holds (well-formed ∘ ,(unowned-label) e_))))]
            [(redex-match? CPCF-mix* b (term F_))
             (redex-match? CPCF e (term e_))])
  [(decorate _ (let* () e_body)) e_body]
  [(decorate string_1 (let* ([(name x_1 x_!_1) κ_1 e_1] [(name x_2 x_!_1) κ_2 e_2] ...) e_body))
   (decorate
    string_1
    (let* ([x_2 (guard-free string_1 κ_2 (var->label x_2) x_1 (var->label x_1) κ_1 (var->label x_1) e_1)
                (guard-free string_1 e_2 (var->label x_2) x_1 (var->label x_1) κ_1 (var->label x_1) e_1)] ...)
      (guard-free string_1 e_body ,(unowned-label) x_1 (var->label x_1) κ_1 (var->label x_1) e_1)))])

(define-metafunction CPCF-mix*
  [(guard-free "b/o" e/κ_i l_owner-e x_free-var l_free-var κ l_owner-κ e_def)
   (substitute e/κ_i x_free-var (mon (l_free-var l_owner-e l_owner-κ)
                                     (decorate/c κ l_owner-κ l_owner-e l_free-var)
                                     (own e_def l_free-var)))]
  [(guard-free "b" e/κ_i l_owner-e x_free-var l_free-var κ l_owner-κ e_def)
   (substitute e/κ_i x_free-var (mon (l_free-var l_owner-e l_owner-κ)
                                     κ
                                     e_def))])

(define-metafunction CPCF-mix*
  [(decorate/c (flat e) l_j l_client l_server)
   (flat-ob (own e l_j) (l_server))]
  [(decorate/c (-> κ_1 κ_2) l_j l_client l_server)
   (-> (decorate/c κ_1 l_j l_server l_client)
       (decorate/c κ_2 l_j l_client l_server))]
  [(decorate/c (->d κ_1 (λ (x : t) κ_2)) l_j l_client l_server)
   (->d            (decorate/c κ_1 l_j l_server l_client)
                   (λ (x : t) (decorate/c κ_2 l_j l_client l_server)))])

(define-metafunction CPCF-O
  undecorate : e -> (name new _)
  #:post ,(redex-match? CPCF-I e (term new))
  [(undecorate (λ (x : t) e)) (λ (x : t) (undecorate e))]
  [(undecorate (e_1 e_2)) ((undecorate e_1) (undecorate e_2))]
  [(undecorate (μ (x : t) e)) (μ (x : t) (undecorate e))]
  [(undecorate (op e_1 e_2)) (op (undecorate e_1) (undecorate e_2))]
  [(undecorate (zero? e_1)) (zero? (undecorate e_1))]
  [(undecorate (if e_1 e_2 e_3)) (if (undecorate e_1) (undecorate e_2) (undecorate e_3))]
  [(undecorate (mon (k l j) κ e)) (mon (k l j) (undecorate/c κ) (undecorate e))]
  [(undecorate (own e l)) (undecorate e)]
  [(undecorate (check (k l) e v)) (check (k l) (undecorate e) (undecorate v))]
  [(undecorate e) e])

(define-metafunction CPCF-O
  [(undecorate/c (flat-ob e (l ...))) (flat (undecorate e))]
  [(undecorate/c (-> κ_1 κ_2)) (-> (undecorate/c κ_1) (undecorate/c κ_2))]
  [(undecorate/c (->d κ_1 (λ (x : t) κ_2))) (->d (undecorate/c κ_1) (λ (x : t) (undecorate/c κ_2)))])
