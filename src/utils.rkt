#lang racket
(provide δ
         closest-label
         unowned-label
         term-number/label?
         term-match/label?
         term-number?
         label-match?
         term-different/label?
         get-value
         substitute/c
         contains?
         var->label
         free-vars
         apply-reduction-relation*/n)

(require redex
         "languages.rkt")

(define-metafunction source
  δ : (op  c   c) -> c
  [(δ (+   n_1 n_2)) ,(+   (term n_1) (term n_2))]
  [(δ (-   n_1 n_2)) ,(-   (term n_1) (term n_2))]
  [(δ (and b_1 b_2)) ,(and (term b_1) (term b_2))]
  [(δ (or  b_1 b_2)) ,(or  (term b_1) (term b_2))]
  [(δ (*   n_1 n_2)) ,(*   (term n_1) (term n_2))]
  [(δ (<   n_1 n_2)) ,(<   (term n_1) (term n_2))]
  [(δ (>   n_1 n_2)) ,(>   (term n_1) (term n_2))]
  [(δ (=   n_1 n_2)) ,(=   (term n_1) (term n_2))])

(define-metafunction CPCF-O
  closest-label-helper : E                   l ->  l
  [(closest-label-helper hole                l)    l]
  [(closest-label-helper (E e)               l)    (closest-label-helper E l)]
  [(closest-label-helper (v E)               l)    (closest-label-helper E l)]
  [(closest-label-helper (op E e)            l)    (closest-label-helper E l)]
  [(closest-label-helper (op v E)            l)    (closest-label-helper E l)]
  [(closest-label-helper (zero? E)           l)    (closest-label-helper E l)]
  [(closest-label-helper (if E e_1 e_2)      l)    (closest-label-helper E l)]
  [(closest-label-helper (mon (l_2 k j) κ E) l_1)  (closest-label-helper E l_2)]
  [(closest-label-helper (check (k l_2) E v) l_1)  (closest-label-helper E l_2)]
  [(closest-label-helper (own E l_2)         l_1)  (closest-label-helper E l_2)])

(define unowned-label (make-parameter "unowned"))
(define-metafunction CPCF-O
  [(closest-label E) (closest-label-helper E ,(unowned-label))])

(define-metafunction CPCF-O
  term-number/label? : v l -> boolean
  [(term-number/label? (own v l_2) l_1)
   ,(and (equal? (term l_1) (term l_2))
         (term (term-number/label? v l_2)))]
  [(term-number/label? n l) #t]
  [(term-number/label? v l) #f])

(define-metafunction CPCF-O
  term-match/label? : v v l -> boolean
  [(term-match/label? v_1 (own v_2 l_2) l_1)
   ,(and (equal? (term l_1) (term l_2))
         (term (term-match/label? v_2 v_1 l_1)))]
  [(term-match/label? v_1 v_2 l)
   ,((default-equiv) (term v_2) (term v_1))])

(define-metafunction CPCF-O
  term-number? : v -> boolean
  [(term-number? (own v l) l_1)
   (term-number? v)]
  [(term-number? n)
   #t]
  [(term-number? v)
   #f])

(define-metafunction CPCF-O
  label-match? : e l -> boolean
  [(label-match? (own v l_2) l_1)
   ,(and ((default-equiv) (term l_1) (term l_2))
         (term (label-match? v l_1)))]
  [(label-match? v l)
   #t])

(define-metafunction CPCF-O
  term-different/label? : v v l -> boolean
  [(term-different/label? v_1 (own v_2 l_2) l_1)
   ,(and (equal? (term l_1) (term l_2))
         (term (term-different/label? v_2 v_1 l_1)))]
  [(term-different/label? v_1 v_2 l_1)
   ,(not ((default-equiv) (term v_2) (term v_1)))])

(define-metafunction CPCF-O
  get-value : v    ->    v
  [(get-value (own v l)) (get-value v)]
  [(get-value v)         v])

(define-metafunction CPCF-O
  substitute/c : κ x e -> κ
  [(substitute/c (flat-ob (own e_2 l_2) (l_ob ...)) x e_1)
   (flat-ob (substitute (own e_2 l_2) x (own e_1 l_2)) (l_ob ...))]
  [(substitute/c (-> κ_1 κ_2) x e)
   (-> (substitute/c κ_1 x e) (substitute/c κ_2 x e))]
  [(substitute/c (->d κ_1 (λ (x_1 : t) κ_2)) x_1 e)
   (->d (substitute/c κ_1 x_1 e) (λ (x_1 : t) κ_2))]
  [(substitute/c (->d κ_1 (λ (y : t) κ_2)) x e)
   (->d (substitute/c κ_1 x e) (λ (y : t) (substitute/c κ_2 x e)))])

(define-metafunction CPCF-O-Γ
  contains? : Γ x l -> boolean
  [(contains? ∘ x l) #f]
  [(contains? (Γ ∪ x : l_1) y l_2)
   ,(or
     (and (equal? (term x) (term y))
          (equal? (term l_1) (term l_2)))
     (term (contains? Γ y l_2)))])

(define-metafunction PCF
  [(different x x) #f]
  [(different x y) #t])

(define-metafunction PCF
  contains-free-var? : e x -> boolean
  [(contains-free-var? c x) #f]
  [(contains-free-var? (λ (x : t) e) y)
   ,(and (term (different x y))
         (term (contains-free-var? e y)))]
  [(contains-free-var? x y)
   ,(not (term (different x y)))]
  [(contains-free-var? (e_1 e_2) x)
   ,(or (term (contains-free-var? e_1 x))
        (term (contains-free-var? e_2 x)))]
  [(contains-free-var? (μ (x : t) e) y)
   ,(or (term (different x y))
        (term (contains-free-var? e x)))]
  [(contains-free-var? (op e_1 e_2) x)
   ,(or (term (contains-free-var? e_1 x))
        (term (contains-free-var? e_2 x)))]
  [(contains-free-var? (zero? e) x)
   (contains-free-var-x e x)]
  [(contains-free-var? (if e_1 e_2 e_3) x)
   ,(or (term (contains-free-var? e_1 x))
        (term (contains-free-var? e_2 x))
        (term (contains-free-var? e_3 x)))])

(define label-regexp (regexp "^(.+)«([0-9]+)([☺☹]+)?»$"))
(define-metafunction CPCF
  var->label : x -> l
  [(var->label x) ,(match (symbol->string (term x))
                     [(regexp #rx"^(.+)«([0-9]+)»$" (list _ base-name index))
                      base-name]
                     [(regexp #rx"^(.+)«([0-9]+)([☺☹]+)»$" (list _ base-name index smiley-number))
                      base-name]
                     [x x])])

(define-metafunction CPCF-O
  free-vars : e -> (x ...)
  [(free-vars c) ()]
  [(free-vars (λ (x : t) e))
   (/ (free-vars e) (x))]
  [(free-vars x) (x)]
  [(free-vars (e_1 e_2 ...))
   (∪ (free-vars e_1) (free-vars e_2) ...)]
  [(free-vars (μ (x : t) e))
   (/ (free-vars e) (x))]
  [(free-vars (op e_1 e_2))
   (∪ (free-vars e_1) (free-vars e_2))]
  [(free-vars (zero? e))
   (free-vars e)]
  [(free-vars (if e_1 e_2 e_3))
   (∪ (free-vars e_1) (free-vars e_2) (free-vars e_3))]
  [(free-vars (own e l))
   (free-vars e)]
  [(free-vars (mon (_ _ _) κ e))
   (∪ (free-vars/c κ) (free-vars e))])

(define-metafunction CPCF-O
  free-vars/c : κ -> (x ...)
  [(free-vars/c (flat-ob e (l_ob ...)))
   (free-vars e)]
  [(free-vars/c (-> κ_1 κ_2))
   (∪ (free-vars/c κ_1) (free-vars/c κ_2))]
  [(free-vars/c (->d κ_1 (λ (x_1 : t) κ_2)))
   (∪ (free-vars/c κ_1) (/ (free-vars/c κ_2) (x_1)))])

(define-metafunction CPCF-O
  ∪ : (x ...) ... -> (x ...)
  [(∪ (x_1 ...) (x_2 ...) (x_3 ...) ...)
   (∪ (x_1 ... x_2 ...) (x_3 ...) ...)]
  [(∪ (x_1 ...))
   (x_1 ...)]
  [(∪) ()])

(define-metafunction CPCF-O
  / : (x ...) (x ...) -> (x ...)
  [(/ (x ...) ()) (x ...)]
  [(/ (x_1 ... x_2 x_3 ...) (x_2 x_4 ...))
   (/ (x_1 ... x_3 ...) (x_2 x_4 ...))
   (side-condition (not (memq (term x_2) (term (x_3 ...)))))]
  [(/ (x_1 ...) (x_2 x_3 ...))
   (/ (x_1 ...) (x_3 ...))])

(define (apply-reduction-relation*/n red-rel term n [acc '()])
  (if (= n 0)
      (cons term acc)
      (let ([reduced (apply-reduction-relation red-rel term)])
        (cond
          [(empty? reduced) (cons term acc)]
          [(= (length reduced) 1) (apply-reduction-relation*/n red-rel (first reduced) (sub1 n) (cons term acc))]
          [else (error 'apply-reduction-relation*/n "expected deterministic reduction relation, given ~a" red-rel)]))))
