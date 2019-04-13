#lang racket/base
(require "stack.rkt"
         "util.rkt")

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

(define (%dump stack)
  (push! stack (dump stack))
  stack)
(provide (rename-out [%dump dump]))

(define (clear stack)
  (clear! stack)
  stack)
(provide clear)

(define (empty? stack)
  (push! stack (empty-stack? stack))
  stack)
(provide empty?)

(provide/qkstack [zero? 1 -> 1] [sub1 1 -> 1] [add1 1 -> 1]
                 [+ 2 -> 1] [- 2 -> 1] [* 2 -> 1] [/ 2 -> 1]
                 [modulo 2 -> 1]
                 [= 2 -> 1]
                 [< 2 -> 1] [> 2 -> 1]
                 [<= 2 -> 1] [>= 2 -> 1]
                 [displayln 1 -> 0]
                 [write 1 -> 0]
                 [not 1 -> 1]
                 [eq? 2 -> 1]
                 [eqv? 2 -> 1]
                 [equal? 2 -> 1])
