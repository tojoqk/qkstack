#lang racket

(struct stack ([data #:mutable]))

(define (make-stack) (stack '()))
(provide make-stack)

(define (stack-empty? s)
  (null? (stack-data s)))
(provide stack-empty?)

(define (pop! stack)
  (let ([data (stack-data stack)])
    (set-stack-data! stack (cdr data))
    (car data)))
(provide pop!)

(define (push! stack value)
  (set-stack-data! stack
                   (cons value (stack-data stack))))
(provide push!)

(define (clear! stack)
  (set-stack-data! stack '()))
(provide clear!)

(define (dump stack)
  (reverse (stack-data stack)))
(provide dump)

(define (load! stack lst)
  (clear! stack)
  (let loop ([lst lst])
    (unless (null? lst)
      (push! stack (car lst))
      (loop (cdr lst))))
  stack)
(provide load!)
