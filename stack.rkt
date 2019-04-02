#lang racket

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
  (set! stack (append lst stack)))
(provide push-list!)

(define (list->stack lst)
  lst)
(provide list->stack)

(define (stack->list stack)
  stack)
(provide stack->list)
