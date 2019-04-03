#lang racket
(require (only-in srfi/1 append-reverse))

(define (make-stack) '())
(provide make-stack)

(define (stack-empty? s) (null? s))
(provide stack-empty?)

(define-syntax-rule (pop! stack)
  (begin0 (car stack)
    (set! stack (cdr stack))))
(provide pop!)

(define-syntax-rule (push! stack value)
  (set! stack (cons value stack)))
(provide push!)

(define-syntax-rule (push-list! stack lst)
  (set! stack (append-reverse lst stack)))
(provide push-list!)

(define (list->stack lst)
  (reverse lst))
(provide list->stack)

(define (stack->list stack)
  (reverse stack))
(provide stack->list)
