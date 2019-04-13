#lang racket/base
(require racket/contract)

(module+ test
  (require rackunit))

(struct stack ([data #:mutable]))
(provide stack?)

(define/contract (make-stack)
  (-> stack?)
  (stack '()))
(provide make-stack)

(define (empty-stack? s)
  (null? (stack-data s)))
(provide (contract-out [empty-stack?
                        (-> empty-stack? boolean?)]))

(define (non-empty-stack? s)
  (not (empty-stack? s)))
(provide (contract-out [non-empty-stack?
                        (-> stack? boolean?)]))

(define (push! s x)
  (set-stack-data! s (cons x (stack-data s))))
(provide (contract-out [push! (-> stack? any/c void?)]))

(define/contract (push-list! s lst)
  (-> stack? list? void?)
  (for ([x lst])
    (push! s x)))
(provide push-list!)

(define (pop! s)
  (let ([data (stack-data s)])
    (set-stack-data! s (cdr data))
    (car data)))
(provide (contract-out [pop! (-> non-empty-stack? any/c)]))

(define/contract (clear! s)
  (-> stack? void?)
  (let ([data (stack-data s)])
    (set-stack-data! s '())))
(provide clear!)

(define/contract (dump s)
  (-> stack? list?)
  (reverse (stack-data s)))
(provide dump)

(define/contract (load! s lst)
  (-> stack? list? void?)
  (clear! s)
  (for ([e lst])
    (push! s e)))
(provide load!)

(module+ test
  (define s (make-stack))

  (check-true (empty-stack? s))
  (check-false (non-empty-stack? s))
  (check-equal? (dump s) '())

  (push! s 'a)

  (check-false (empty-stack? s))
  (check-true (non-empty-stack? s))
  (check-equal? (dump s) '(a))

  (push! s 'b)
  (check-equal? (dump s) '(a b))

  (push! s 'c)
  (push! s 'd)

  (check-eq? (pop! s) 'd)
  (check-equal? (dump s) '(a b c))

  (define d (dump s))

  (clear! s)
  (check-equal? (dump s) '())
  (push! s 'foo!)
  (load! s d)

  (check-equal? (dump s) '(a b c))

  (push-list! s '(x y z))

  (check-equal? (dump s) '(z y x a b c)))
