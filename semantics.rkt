
#lang racket

(require math)

(provide (all-defined-out))

;; === NOTES ===

;; the axis operator should "use" suboperators, rather than vice-versa

;; crank out the easy ones

;; confront the lack of array prototypes :(

;; ensure that all indexes begin at 1

;; TODO: is literally everything element supposed to be an array? such that "scalars" are actually arrays of dimension '#() ?

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

(define (array-member x Y)
  (array-ormap (curry eq? x) Y))

(define (array-flip Y)
  (if (and (array? Y) (> (array-dims Y) 1))
      (error "array-flip: Y must be a vector or scalar")
      (array-slice-ref (if (scalar? Y) (array Y) Y) (list (:: #f #f -1)))))

(define (one? Y)
  (eq? 1 Y))

(define (boolean->int x)
  (if x 1 0))

(define (logical-function f)
  (λ (X Y)
    (if (and (or (zero? X) (one? X))
             (or (zero? Y) (one? Y)))
        (boolean->int (f (one? X) (one? Y)))
        (error "logical-function: elements must be 0s and 1s"))))

;; filters members of X based on (f x Y)
(define (array-all-filter f X Y [start (array #[])])
  (if (and (array? X) (> 1 (array-dims X)))
      (error "array-all-filter: X must be a scalar or vector")
        (array-flip
         (array-all-fold (if (scalar? X) (make-array #(1) X) X)
                         (λ (x y)
                           (array-append* (list (if (f x Y)
                                                    (array x)
                                                    (array #[]))
                                                y)))
                         (array-flip start)))))

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

(define logical-and (function null (dyad-scalar-guard (logical-function (λ (X Y) (and X Y))))))

;; ∨ LOGICAL OR

(define logical-or (function null (dyad-scalar-guard (logical-function (λ (X Y) (or X Y))))))

;; ⍲ LOGICAL NAND

(define logical-nand (function null (dyad-scalar-guard (logical-function (λ (X Y) (nand X Y))))))

;; ⍱ LOGICAL NOR

(define logical-nor (function null (dyad-scalar-guard (logical-function (λ (X Y) (nor X Y))))))

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

(define (depth Y)
  (if (scalar? Y)
      0
      (add1 (array-all-max (array-map depth Y)))))
(define equal-underbar (function depth (compose boolean->int equal?)))

;; ≢ EQUAL UNDERBAR SLASH

(define equal-underbar-slash (function null (compose boolean->int not equal?)))

;; === STRUCTURAL ===

;; ⍴  RHO


(define (shape-of Y)
  (if (scalar? Y)
      (array #[])
      (vector->array (array-shape Y)))) ;; TODO vector->array necessary?

;; TODO: what if array-dims > 1
(define (reshape X Y)
  (if (and (array? X) (< 1 (array-dims X)))
      (error "reshape: X must be scalar or vector")
      (for/array #:shape (if (scalar? X) (vector X) (array->vector X))
                 ([i (in-cycle (in-array (if (scalar? Y) (array Y) Y)))])
                 i)))

(define rho (function shape-of reshape))

;; ,  COMMA

;; TODO: ravel with axis: array-axis-expand & array-axis-reduce
(define (ravel Y)
  (array-flatten (if (array? Y) Y (array Y))))

;; TODO
(define (catenate X Y) #f)

;; TODO
(define (laminate X Y) #f)

(define comma (function ravel null))

;; ⍪  COMMA BAR

;; ⌽  CIRCLE STILE

;; TODO: slicing might be helpful
(define (array-reverse Y) #f)

;; TODO: there may be a rotate function
(define (rotate Y) #f)

(define circle-stile (function array-reverse rotate))

;; ⊖  CIRCLE BAR

;; ⍉  CIRCRLE BACKSLASH

;; ↑  UP ARROW

(define (array-first Y)
  (if (and (array? Y) (eq? 0 (array-length Y)))
      0 ;; TODO should be based on array prototype
      (array-ref (ravel Y) #(0))))

;; TODO: (array-slice-ref Y (if (positive? X) (:: 0 X 1) (:: (- (array-length Y) X) -1)))
(define (array-take X Y) #f)

(define up-arrow (function array-first array-take))

;; ↓  DOWN ARROW

;; TODO
(define (array-drop X Y) #f)

(define down-arrow (function null array-drop))

;; ⊂  LEFT SHOE

(define (enclose Y)
  (if (array? Y)
      (array Y)
      Y))

;; TODO
(define (array-partition X Y) #f)

(define left-shoe (function enclose array-partition))

;; ≡  EQUAL UNDERBAR

;; ∊  EPSILON

(define (enlist Y)
  (define (array-flatten* Y)
    (if (scalar? Y)
        (array Y)
        (array-all-fold Y
                        (λ (x y)
                          (array-append* (list (array-flatten* x) y)))
                        (array #[]))))
  (array-flip (array-flatten* Y)))


;; === SELECTION & SETS ===

;; ⌷  SQUAD

;; ⊃  RIGHT SHOE

;; TODO: review what "scalar" is supposed to mean
(define (disclose Y) #f)

;; TODO
(define (pick X Y) #f)

(define right-shoe (function disclose pick))

;; /  SLASH

;; ⌿  SLASH BAR

;; \  BACKSLASH

;; ⍀  BACKSLASH BAR

;; ~  TILDE

(define (integer-not Y)
  (if (or (eq? Y 0) (eq? Y 1))
      (if (zero? Y) 1 0) 
      (error "integer-not: elements must be 1 or 0")))

(define (without X Y)
  (array-all-filter (compose not array-member) X Y))

(define tilde (function (monad-scalar-guard integer-not) without))

;; ∪  UP SHOE

(define (unique Y)
  (if (and (array? Y) (eq? 1 (array-dims Y)))
      (list->array (remove-duplicates (array->list Y)))
      (error "unique: argument must be a vector")))

(define (union X Y)
  (if (and (array? X) (> 1 (array-dims X)))
      (error "union: X must be a scalar or vector")
      (let ([X (if (scalar? X) (make-array #(1) X) (array-flip X))])
        (array-flip
         (array-all-fold Y
                         (λ (x y)
                           (array-append* (list (if (array-member x X)
                                                    (array #[])
                                                    (array x))
                                                y)))
                         X)))))

(define up-shoe (function unique union))

;; ∩  DOWN SHOE

(define (intersection X Y)
  (array-all-filter array-member X Y))

(define down-shoe (function null intersection))

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

(define (membership X Y)
  (cond [(scalar? Y) (error "membership: Y must be a vector")]
        [(scalar? X) (boolean->int (array-member X Y))]
        [else (array-map (compose boolean->int (curryr array-member Y)) X)]))

(define epsilon (function enlist membership))

;; ⍷  EPSILON UNDERBAR

;; TODO
;; (define (find X Y)
;; (define epsilon-underbar (function find null))

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
