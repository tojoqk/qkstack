#lang racket
(require "stack.rkt")
(require "util.rkt")

;; stack processing
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

(define (%dump stack)
  (push! stack (dump stack))
  stack)
(provide (rename-out [%dump dump]))

(define (load stack)
  (load! stack (pop! stack))
  stack)
(provide load)

(define (clear stack)
  (clear! stack)
  stack)
(provide clear)

(define (empty? stack)
  (push! stack (stack-empty? stack))
  stack)
(provide empty?)

;; list processing
(provide/qkstack [cons 2 -> 1]
                 [car 1 -> 1]
                 [cdr 1 -> 1]
                 [null? 1 -> 1]
                 [pair? 1 -> 1])

;; arithmetic
(provide/qkstack [zero? 1 -> 1]
                 [sub1 1 -> 1]
                 [add1 1 -> 1]
                 [+ 2 -> 1]
                 [- 2 -> 1]
                 [* 2 -> 1]
                 [/ 2 -> 1]
                 [modulo 2 -> 1]
                 [quotient 2 -> 1]
                 [= 2 -> 1]
                 [< 2 -> 1]
                 [> 2 -> 1]
                 [<= 2 -> 1]
                 [>= 2 -> 1])

;; I/O
(provide/qkstack [read-line 0 -> 1]
                 [display 1 -> 0]
                 [displayln 1 -> 0]
                 [write 1 -> 0]
                 [not 1 -> 1])

;; equality
(provide/qkstack [eq? 2 -> 1]
                 [eqv? 2 -> 1]
                 [equal? 2 -> 1])
