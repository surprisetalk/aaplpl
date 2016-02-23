
#lang racket

(require math)

(provide (all-defined-out))

;; === NOTES ===


;; === META ===

(define (function [monad null] [dyad null])
  (case-lambda
    [()    (error "no arguments given")]
    [(x)   (if monad
               (monad x)
               (error "no monad defined"))]
    [(x y) (if dyad
               (dyad x y)
               (error "no dyad defined"))]
    [r     (error "too many arguments given")]))

;; TODO
;; moderates a function
;; (define (operator a-function)


;; === GUARDS ===

(define (scalar? Y)
  (not (array? Y)))

(define (monad-scalar-guard f)
  (λ (Y) (if (array? Y)
             (array-map f Y)
             (f Y))))

(define (dyad-scalar-guard f)
  (λ (X Y)
    (cond [(and (scalar? X) (scalar? Y))  (f X Y)]
          [(scalar? X) (array-map (curry  f X) Y)]
          [(scalar? Y) (array-map (curryr f Y) X)]
          [(equal? (array-shape X) (array-shape Y)) (array-map (dyad-scalar-guard f) X Y)] ;; TODO: recursive scalar guard?
          [else (error "dyad-scalar-guard: shape error")])))


;; === HELPERS ===

;; TODO: (eq? d '(1)) is dangerous
;; TODO: what about the empty vector?
(define (~scalar? Y)
  (or (not (array? Y))
      (let ([d (vector->list (array-shape Y))])
        (or (eq? d empty) (eq? d '(1))))))

;; TODO: what about the empty vector?
(define (~array->scalar Y)
  (cond [(scalar? Y) Y]
        [(~scalar? Y) (car (array->list Y))]
        [else (error "~array->scalar: Y must be of rank 0 or 1")]))

(define (boolean->int x)
  (if x 1 0))


;; === MATH ===

;; + PLUS

(define plus (function identity (dyad-scalar-guard +)))

;; - MINUS

(define minus (function (monad-scalar-guard -) (dyad-scalar-guard -)))

;; ÷ DIVIDE

(define divide (function (monad-scalar-guard /) (dyad-scalar-guard /)))

;; × TIMES

(define (sign-of x) (cond [(< x 0) -1] [(> x 0) 1] [else 0]))
(define times (function (monad-scalar-guard sign-of) (dyad-scalar-guard *)))

;; ⌈ UPSTILE

(define upstile (function (monad-scalar-guard ceiling) (dyad-scalar-guard max)))

;; ⌊ DOWNSTILE

(define downstile (function (monad-scalar-guard floor) (dyad-scalar-guard min)))

;; * STAR

(define star (function (monad-scalar-guard exp) (dyad-scalar-guard expt)))

;; ! EXCLAMATION 

(define exclamation (function (monad-scalar-guard gamma) (dyad-scalar-guard (curryr binomial))))

;; | STILE

(define (residue X Y)
  (if (zero? X) 0 (modulo Y X)))
(define stile (function (monad-scalar-guard abs) (dyad-scalar-guard residue)))

;; ⍟ CIRCLE FILL

(define circle-fill (function (monad-scalar-guard log) (dyad-scalar-guard (λ (X Y) (/ (log Y) (log X))))))

;; ○ CIRCLE

(define (circular-hyperbolic ⍺ ⍵)
  ((case ⍺
         [(0) (λ (x) (sqrt (- 1 (sqr x))))] ;; pythagorean function
         [(1) sin] [(-1) asin]
         [(2) cos] [(-2) acos]
         [(3) tan] [(-3) atan]
         [(4) (λ (x) (not (eq? (+ 1 (sqr x)) 0.5)))] [(-4) (λ (x) (sqrt (+ -1 (sqr x))))] ;; pythagorean functions
         [(5) sinh] [(-5) (error "circle: arcsinh not implemented yet")] 
         [(6) cosh] [(-6) (error "circle: arccosh not implemented yet")] 
         [(7) tanh] [(-7) (error "circle: arctanh not implemented yet")] 
         [else (error "circle: left operand must be a value -7 <= x <= 7")])
   ⍵))
(define circle (function (monad-scalar-guard (curry * pi)) circular-hyperbolic))

;; ⌹ DOMINO

(define (matrix-divide X Y)
  (define (->matrix Y)
    (if (scalar? Y)
        (make-array #(1 1) Y)
        (let ([d (array-dims Y)])
          (cond [(eq? 0 d) (make-array #(1 1) Y)]
                [(eq? 1 d) (->col-matrix Y)] 
                [(eq? 2 d) Y]
                [else (error "->matrix: array must have dimension < 3")]))))
  (let ([x (->matrix X)]
        [y (->matrix Y)])
    (matrix* (matrix-inverse y) x)))
    ;; ((inner-product plus times) (matrix-inverse y) x)))
;; TODO apl guide demands inversion of nonsquare matrices (using least squares?) ( ಠ╭╮ಠ)  
(define domino (function (λ (Y) (if (scalar? Y) (/ Y) (matrix-inverse Y))) matrix-divide))

;; ⊥ UP TACK

;; ⊤ DOWN TACK

;; ? QUESTION MARK

(define (roll Y) (ceiling (random Y)))
(define (deal X Y)
  (if (and (~scalar? X) (~scalar? Y))
      (let ([x (~array->scalar X)][y (~array->scalar Y)])
        (if (<= x y)
            (take (shuffle (range 1 (add1 y))) x)
            (error "deal: X must be less than or equal to Y")))
      (error "deal: X and Y must be of rank 0 or 1")))
(define question (function (monad-scalar-guard roll) deal))


;; === LOGIC & COMPARISON ===

;; ∧ LOGICAL AND

;; NOTE: (and 3 4 5) -> 5, (and 3 0 5) -> 0

;; ∨ LOGICAL OR

;; NOTE: (and 3 4 5) -> 3, (and 0 0 0) -> 0

;; ⍲ LOGICAL NAND

;; ⍱ LOGICAL NOR

;; < LESS THAN

(define less-than (function null (dyad-scalar-guard <)))

;; > GREATER THAN

(define greater-than (function null (dyad-scalar-guard >)))

;; ≤ LESS THAN OR EQUAL TO

(define less-than-or-equal-to (function null (dyad-scalar-guard <=)))

;; ≥ GREATER THAN OR EQUAL TO

(define greater-than-or-equal-to (function null (dyad-scalar-guard >=)))

;; = EQUAL

;; TODO: only supposed to compare elements, not arrays
(define equal (function null (dyad-scalar-guard (compose boolean->int equal?))))

;; ≠ NOT EQUAL

(define not-equal (function null (dyad-scalar-guard (compose boolean->int not equal?))))

;; ≡ EQUAL UNDERBAR

;; TODO
(define (depth Y)
  (if (scalar? Y)
      0
      (add1 (array-all-max (array-map depth Y)))))
(define equal-underbar (function depth (compose boolean->int equal?)))

;; ≢ EQUAL UNDERBAR SLASH

(define equal-underbar-slash (function null (compose boolean->int not equal?)))

;; === STRUCTURAL ===

;; ⍴  RHO

;; ,  COMMA

;; ⍪  COMMA BAR

;; ⌽  CIRCLE STILE

;; ⊖  CIRCLE BAR

;; ⍉  CIRCRLE BACKSLASH

;; ↑  UP ARROW

;; ↓  DOWN ARROW

;; ⊂  LEFT SHOE

;; ≡  EQUAL UNDERBAR

;; ∊  EPSILON

;; TODO: make sure the array is flattened in the right order
;; (define (membership X Y)
;;   (cond [(scalar? Y) (error "membership: Y must be a vector")]
;;         [(scalar? X) (boolean->int (member X Y))]
;;         [else (map (compose boolean->int (curryr member Y)) X)]))
;; TODO: empty vectors not supposed to appear in the result
;; (define epsilon (function enlist membership))

;; === SELECTION & SETS ===
;; ⌷  SQUAD
;; ⊃  RIGHT SHOE
;; /  SLASH
;; ⌿  SLASH BAR
;; \  BACKSLASH
;; ⍀  BACKSLASH BAR
;; ~  TILDE
;; ∪  UP SHOE
;; ∩  DOWN SHOE
;; ⊣  LEFT TACK
;; ⊢  RIGHT TACK

;; === SEARCH & SORT ===

;; ⍳  IOTA

(define (index-gen Y)
  (if (~scalar? Y)
      (let ([y (~array->scalar Y)])
        (build-array (vector y) (compose add1 car vector->list)))
      (error "index-gen: expects scalar or one-element vector")))
(define (index-of X Y)
  (define (find-index X y)
    (let ([e (for/first ([i (in-naturals 1)][x (in-array X)] #:when (eq? x y)) i)])
      (if (eq? #f e) (add1 (array-size X)) e)))
  (cond [(or (scalar? X) (> 1 (array-dims X))) (error "index-of: X must be a vector")]
        [(array? Y) (array-map (curry find-index X) Y)] ;; TODO: double-check the broadcasting behavior here
        [else (find-index X Y)]))
(define iota (function index-gen index-of))

;; ∊  EPSILON

;; ⍷  EPSILON UNDERBAR

;; ⍋  GRADE UP

;; ⍒  GRADE DOWN


;; === MISCELLANEOUS ===
;; ¯  HIGH MINUS
;; '  QUOTE
;; ←  LEFT ARROW
;; ⍬  ZILDE
;; ⍕  THORN
;; ⋄  DIAMOND
;; ⍝  LAMP
;; ∇  DEL
;; ⍺  ALPHA
;; ⍵  OMEGA
;; ⎕  QUAD
;; [] BRACKETS
;; {} BRACES

;; === OPERATORS ===
;; ¨  DIERESIS
;; ⍨  TILDE DIERESIS
;; ⍣  STAR DIERESIS
;; .  DOT
;; ∘  JOT
;; /  SLASH
;; \  BACKSLASH
;; ⌿  SLASH BAR
;; ⍀  BACKSLASH BAR
;; ⌸  QUAD EQUAL
;; ⍤  JOT DIERESIS
;; ⍠  QUAD COLON
