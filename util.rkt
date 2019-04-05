#lang racket
(require "stack.rkt")
(require (for-syntax racket/list))

(define-syntax (define/qkstack stx)
  (syntax-case stx (->)
    [(_ (name arg ... -> 0) body body* ...)
     #`(define (name stack)
         (let* #,(reverse (syntax->list #'([arg (pop! stack)] ...)))
           body body* ...)
         stack)]
    [(_ (name arg ... -> n) body body* ...)
     (with-syntax ([(value ...)
                    (generate-temporaries (make-list (syntax->datum #'n) #t))])
       #`(define (name stack)
           (define-values (value ...)
             (let* #,(reverse (syntax->list #'([arg (pop! stack)] ...)))
               body body* ...))
           (push! stack value) ...
           stack))]
    [(_ (name arg ...) body body* ...)
     #'(define/qkstack (name arg ... -> 1) body body* ...)]))
(provide define/qkstack)

(define-syntax (provide/qkstack-1 stx)
  (syntax-case stx (->)
    [(_ name (m -> n))
     (with-syntax ([(arg ...) (generate-temporaries (make-list (syntax->datum #'m) #t))])
       #'(begin
           (define/qkstack (gname arg ... -> n)
             (name arg ...))
           (provide (rename-out [gname name]))))]))

(define-syntax-rule (provide/qkstack [name (m -> n)] ...)
  (begin
    (provide/qkstack-1 name (m -> n))
    ...))
(provide provide/qkstack)
