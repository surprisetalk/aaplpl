
#lang racket

;; TODO
;; (require rackunit)                ;; for unit testing

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

(define ? (function roll deal))
(define (roll a-list)
  (car (shuffle a-list)))
(define (deal an-integer a-list)
  (roll (take a-list an-integer)))

