
#lang racket

;; TODO: vector vs array?
;; TODO: find all potential scalar errors
;; TODO: test each function (in real APL interpreter) for X:scalar,array Y:scalar,array
;; TODO: function reference, starting at pg 91: http://microapl.com/apl/APLXLangRef.pdf 

;; TODO
;; (require rackunit)                ;; for unit testing
(require math)

;; === NOTES ===

;; === META ===

(provide (all-defined-out))

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
  (λ (x) (if (list? x)
             (map f x)
             (f x))))
      

;; f should be a λ that accepts two scalars
(define (dyad-scalar-guard f)
  (λ (x y)
    (cond
      [(and (list? x) (list y)) (map-zip f x y)]
      [(list? y) (map-deep (curry  f x) y)]
      [(list? x) (map-deep (curryr f y) x)]
      [else (f x y)])))


;; === HELPERS ===

;; TODO: type-check X and Y
(define (zip X Y)
  (for/list ([x X] [y Y])
    (list x y)))

;; TODO: what happens if items are differently shaped?
(define (map-zip f X Y)
  (map (curry (λ (x y) (if (or (list? x)
                               (list? y))
                           (map-zip f x y)
                           (f x y))))
       (zip X Y)))

(define (map-deep f X)
  (map (λ (x) (if (list? x)
                  (map-deep f x)
                  (f x)))
       X))

(define (boolean->int x)
  (if x 1 0))

(define (int->boolean x)
  (case x [(0) #f]
        [(1) #t]
        [else (error (string-append "int->boolean: '" (string x) "' must be 1 or 0"))]))

;; === PRIMITIVES ===

;; BUG: + is a racket primitive
(define '+ (function identity (dyad-scalar-guard +)))

;; BUG: - is a racket primitive
(define '- (function (monad-scalar-guard negate) (dyad-scalar-guard -)))

(define '× (function (monad-scalar-guard signof) (dyad-scalar-guard *)))
(define (signof x) (cond [(< x 0) -1] [(> x 0) 1] [else 0]))

(define '÷ (function (monad-scalar-guard /) (dyad-scalar-guard /)))

(define '⌈ (function (monad-scalar-guard ceiling) (dyad-scalar-guard max)))

(define '⌊ (function (monad-scalar-guard floor) (dyad-scalar-guard min)))

(define '| (function (monad-scalar-guard abs) (dyad-scalar-guard modulo)))

;; === ALGEBRAIC ===

(define '⍳ (function (monad-scalar-guard index-gen) index-of))
(define (index-gen x) (range 1 (add1 x)))
(define (index-of X Y)
  (define (find a-list an-item)
    (for/first ([x a-list] #:when (eq? x an-item)) x))
  (cond [(not (list? X)) (error "index-of: left operand must be an array")] ;; TODO: should this be true?
        [(list? Y)
         (map (curry find X) Y)]
        [else (find X y)]))

(define '? (function (monad-scalar-guard roll) deal))
(define (roll x) (floor (random x)))
;; TODO: what if x>|Y
(define (deal x Y)
  (cond [(list? x) (error "deal: left operand must be a scalar")] ;; TODO: should this be true?
        [(list? Y) (take (shuffle Y) x)]
        [else (build-list roll x)])) ;; BUG: without replacement

(define '* (function (monad-scalar-guard exp) (dyad-scalar-guard expt)))

(define '⍟ (function (monad-scalar-guard log) (dyad-scalar-guard logb)))
(define (logb x b) (/ (log x) (log b)))

(define '○ (function (monad-scalar-guard (curry * pi)) circle))
(define (circle a w)
  ((case a
         [(0) (λ (x) (sqrt (- 1 (sqr x))))]
         [(1) sin] [(-1) asin]
         [(2) cos] [(-2) acos]
         [(3) tan] [(-3) atan]
         [(4) (λ (x) (not (eq? (+ 1 (sqr x)) 0.5)))] [(-4) (λ (x) (sqrt (+ -1 (sqr x))))]
         [(5) sinh] [(-5) (error "circle: arcsinh not implemented yet")] 
         [(6) cosh] [(-6) (error "circle: arccosh not implemented yet")] 
         [(7) tanh] [(-7) (error "circle: arctanh not implemented yet")] 
         [(8 9 10 11 12 -8 -9 -10 -11 -12) (error "circle: not implemented yet")]
         [else (error "circle: left operand must be a value -12 <= x <= -12")])
   w))

(define '! (function (monad-scalar-guard gamma) (dyad-scalar-guard binomial))) ;; TODO: binomial might have reversed arguments

(define '⌹ (function inverse divide))
(define (inverse X)
  (if (and (list? X) (list? (car X)))
      (matrix->list (matrix-inverse (matrix X))) ;; BUG: matrix->list flattens the matrix into single-depth list; create matrix->lists
      (error "matrix-inverse: right operand must be matrix")))
(define (divide X Y) ;; TODO is there more functionality?
  ;; TODO check if they're lists-of-lists
  (matrix->list (matrix* (matrix X) (matrix-inverse (matrix Y))))) ;; BUG: matrix->list flattens the matrix into single-depth list; create matrix->lists



;; === CONDITIONAL ===

;; TODO: #t & #f -> 1 & 0

(define '< (function null (dyad-scalar-guard (compose boolean->int <))))
(define '≤ (function null (dyad-scalar-guard (compose boolean->int <=))))
(define '= (function null (dyad-scalar-guard (compose boolean->int eq?))))
(define '< (function null (dyad-scalar-guard (compose boolean->int >))))
(define '≥ (function null (dyad-scalar-guard (compose boolean->int >=))))
(define '≡ (function depth eq?))
(define (depth X)
  (if (list? X)
      (add1 (apply max (map depth X)))
      0))
(define '≢ (function length (compose boolean->int not eq?))) ;; TODO: length = 1 if scalar
(define '∊ (function flatten (compose boolean->int member))) ;; TODO: what if flatten doesn't have a list?      
;; TODO: (define '⍷ null find)


;; === LOGICAL ===

(define '~ (function (monad-scalar-guard (compose boolean->int not int->boolean)) (curryr remove*))) ;; TODO remove* cannot accept scalars
(define '∧ (function null (dyad-scalar-guard lcm)))
(define '∨ (function null (dyad-scalar-guard gcd)))
(define '⍲ (function null (dyad-scalar-guard (compose boolean->int nand int->boolean))))
(define '⍱ (function null (dyad-scalar-guard (compose boolean->int nor int->boolean))))


;; === MANIPULATION & SELECTION ===

(define '⍴ (function shape reshape))
;; TODO: misshapen arrays should return empty
(define (shape X)
  (if (list? X)
      (cons (length X)
            (shape (car X)))
      empty))
;; holy crap this is beautiful
;; TODO: X is scalar? etc
(define (reshape X Y)
  (for/list ([i (car X)]
             [y (foldr (curry in-slice)
                       (in-cycle Y)
                       X)])
    y)) ;; TODO: maybe foldl? maybe (reverse X)?


(define ', (function flatten catenate))
;; TODO: check for size errors; they won't be caught until a map-zip messes up
(define (catenate X Y)
  (cond [(not (list? Y)) (error "catenate: scalar catenation not implemented yet")] 
        [(not (list? (car X))) (append X Y)]
        [else (map-zip catenate X Y)]))

(define '∪ (function remove-duplicates (compose remove-duplicates append)))

(define '∩ (function null intersection))
(define (intersection X Y)
  (cond [(or (empty? X) (empty? Y)) empty]
        [(member (car X) Y) (intersection (cdr X))]
        [else (cons (car X) (intersection (cdr X)))]))
        
;; TODO
(define '⌽ (function reverse (λ (x y) (error "rotate: function not implemented yet"))))

(define '⍉ (function transpose (λ (x y) (error "dyadic transpose: function not implemented yet"))))
(define (transpose X)
  (matrix->list (matrix-transpose (matrix X)))) ;; BUG matrix->list flattens

;; TODO multidimensional take
(define '↑ (function car (curryr take)))

;; TODO multidimensional drop
(define '↓ (function cdr (curryr drop)))

;; (define '⊂ (function list partitioned-enclose))
;; ;; TODO multidimensional
;; (define (partioned-enclose X Y) 
;;   (cond ([(not (eq? (length X) (length Y))) (error "partioned-enclose: length error")]
;;          [(empty? X) '()]
;;          [(eq? 0 (car X)) 
  
      

