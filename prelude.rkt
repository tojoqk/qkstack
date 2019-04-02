#lang racket
(require "stack.rkt")
(require "util.rkt")

(define/qkstack (dup a -> 2) (values a a))
(provide dup)

(define/qkstack (drop _ -> 0) (values))
(provide drop)

(define/qkstack (swap a b -> 2) (values b a))
(provide swap)

(define/qkstack (over a b -> 3) (values a b a))
(provide over)

(define/qkstack (rot a b c -> 3) (values b c a))
(provide rot)

(define/qkstack (-rot a b c -> 3) (values c a b))
(provide -rot)

(define/qkstack (nip a b -> 1) b)
(provide nip)

(define/qkstack (tuck a b -> 3) (values b a b))
(provide tuck)

(provide/qkstack [+ (2 -> 1)]
                 [- (2 -> 1)]
                 [* (2 -> 1)]
                 [/ (2 -> 1)]
                 [modulo (2 -> 1)]
                 [quotient (2 -> 1)]
                 [= (2 -> 1)]
                 [< (2 -> 1)]
                 [> (2 -> 1)]
                 [<= (2 -> 1)]
                 [>= (2 -> 1)]
                 [zero? (1 -> 1)]
                 [sub1 (1 -> 1)]
                 [add1 (1 -> 1)]
                 [display (1 -> 1)]
                 [displayln (1 -> 1)]
                 [car (1 -> 1)]
                 [cdr (1 -> 1)]
                 [cons (2 -> 1)]
                 [null? (1 -> 1)]
                 [not (1 -> 1)]
                 [equal? (2 -> 1)]
                 [eq? (2 -> 1)]
                 [eqv? (2 -> 1)])
