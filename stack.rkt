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


