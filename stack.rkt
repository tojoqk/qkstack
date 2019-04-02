#lang racket

(define (make-stack) '())
(provide make-stack)

(define-syntax-rule (pop! stack)
  (begin0 (car stack)
    (set! stack (cdr stack))))
(provide pop!)

(define-syntax-rule (push! stack value)
  (set! stack (cons value stack)))
(provide push!)

