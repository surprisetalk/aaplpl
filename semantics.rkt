
#lang racket

;; TODO
;; (require rackunit)                ;; for unit testing
(require math)


;; === NOTES ===

;; function reference, starting at pg 91: http://microapl.com/apl/APLXLangRef.pdf 

;; http://www.sacrideo.us/apl-a-day-2-arrays-values/

#|
    Data in APL is arranged in arrays. 
    An array is a collection of data with a number of dimensions (rank) and a number of elements in each dimension (shape). 
    Some or all of the elements may themselves be arrays, making the array a nested array with a third property, depth.
|#
  
#|
    Rank       Name        Dimensions
    0          Scalar      None (one element only)
    1          Vector      1    (elements)
    2          Matrix      2    (rows and columns)
    3                      3    (planes, rows, and columns)
    4                      4    (blocks, planes, rows, and columns)
|#
  
#|
    Depth      Description
    0          Simple scalar
    1          Simple array
    2          Deepest element in the array is of depth 1
    3          Deepest element in the array is of depth 2
    .
    n          Deepest element in the array is of depth n-1
|#
  
#|
    Certain functions require the addition of 'fill' elements to arrays, for example the functions ↑ ('take'), \ ('expand') and / ('replicate'). 
    These function can add extra elements to an existing array; the prototype is used to determine the type and shape of the extra elements.

    Array Type             Fill Element
    Numeric                Zero
    Character              Space
    Nested or mixed        Prototype or first element, with numbers replaced by zeroes and characters by spaces
|#


;; === META ===

(provide (all-defined-out))

;; TODO: explore the equivalence of one-element arrays and scalars
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

(define (monad-scalar-guard f)
  (λ (X) (if (list? X)
             (map f X)
             (f X))))

;; TODO: clean this up
(define (dyad-scalar-guard f)
  (λ (X Y)
    (cond [(and (scalar? X) (scalar? Y)) (f X Y)]
          [(scalar? X) (map-deep (curry f X) Y)]
          [(scalar? Y) (map-deep (curryr f Y) X)]
          [(same-length? X Y) (map (λ (XY) ((dyad-scalar-guard f)
                                            (car XY)
                                            (cdr XY)))
                                   (zip X Y))]
          [else (error "dyad-scalar-guard: shape error")])))


;; === HELPERS ===

(define (boolean->int x)
  (if x 1 0))

(define (scalar? Y)
  (not (list? Y)))

(define (same-length? X Y)
  (and (list? X)
       (list? Y)
       (eq? (length X) (length Y))))

(define (zip X Y)
  (if (and (list? X) (list? Y))
      (for/list ([x X] [y Y])
        (list x y))
      (error "zip: expects two lists")))

(define (map-deep f Y)
  (if (list? Y)
      (map (curry map-deep f) Y)
      (f Y)))
  

;; === MATH ===

;; + PLUS

(define plus (function identity (dyad-scalar-guard +)))

;; - MINUS

(define minus (function (monad-scalar-guard (curry * -1)) (dyad-scalar-guard -)))

;; ÷ DIVIDE

(define divide (function (monad-scalar-guard /) (dyad-scalar-guard /)))

;; × TIMES

(define times (function (monad-scalar-guard sign-of) (dyad-scalar-guard *)))
(define (signof x) (cond [(< x 0) -1] [(> x 0) 1] [else 0]))

;; ⌈ UPSTILE

(define upstile (function (monad-scalar-guard ceiling) (dyad-scalar-guard max)))

;; ⌊ DOWNSTILE

(define upstile (function (monad-scalar-guard floor) (dyad-scalar-guard min)))

;; * STAR

(define star (function (monad-scalar-guard exp) (dyad-scalar-guard expt)))

;; ! EXCLAMATION 

;; TODO
(define exclamation (function (monad-scalar-guard gamma) null)
(define (binomial X Y) null)

;; | STILE

(define stile (functon (monad-scalar-guard abs) (dyad-scalar-guard modulo)))

;; ⍟ LOG

(define log (function (monad-scalar-guard log) (dyad-scalar-guard (λ (X Y) (/ (log X) (log Y))))))

;; ○ CIRCLE

;;   ALPHA
;;   OMEGA
(define circle (function (monad-scalar-guard (curry * pi) circular-hyperbolic) 
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

;; ⌹ DOMINO

;; TODO

;; ⊥ UP TACK

;; ⊤ DOWN TACK

;; ? QUESTION MARK

(define (function (monad-scalar-guard roll) deal))
(define (roll Y) (ceiling (random Y)))
(define (deal X Y)
  (if (and (scalar? X) (scalar? Y))
      (if (<= X Y)
          (take (shuffle (range 1 (add1 Y))) X)
          (error "deal: X must be less than or equal to Y"))
      (if (or (and (list? X) (> (length X) 1))
              (and (list? Y) (> (length Y) 1)))
          (error "deal: X and Y must be rank 1 or 0")
          (deal (if (list? X) (car X) X) (if (list? Y) (car Y) Y)))))


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
(define equal (function null (dyad-scalar-guard (compose booolean->int eq?))))

;; ≠ NOT EQUAL

(define not-equal (function null (dyad-scalar-guard (compose boolean->int not eq?))))

;; ≡ EQUAL UNDERBAR

;; TODO
(define equal-underbar (function depth (compose boolean->int eq?)))
(define (depth X Y) 0) 

;; ≢ EQUAL UNDERBAR SLASH

(define equal-underbar (function null (compose boolean->int not eq?)))

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
(define epsilon (function flatten membership))
(define (membership X Y) 0)
;; TODO: the result is always the same shape as the left-hand argument
  ;; (cond [(scalar? Y) (error "membership: Y must be a vector")]
  ;;       [(scalar? X) (boolean->int (member X Y))]
  ;;       [else (map (curryr (compose boolean->int member) Y) X)]))

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

(define iota (function index-gen index-of))
(define (index-gen Y)
  (cond [(zero? Y) empty]
        [(scalar? Y) (range 1 (add1 Y))]
        [(eq? 1 (length Y)) (index-gen (car Y))]
        [else (error "index-gen: expects scalar or one-element vector")]))
(define (index-of X Y)
  (define (find a-list an-item)
    (for/first ([x a-list] #:when (eq? x an-item)) x))
  (cond [(scalar? X) (index-of (list X) Y)]
        [(scalar? Y) (let ([x (find X Y)])
                       (if x x (add1 (length X))))]
        [else (map (curry find X) Y)]))
        

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
