#lang racket
(require "stack.rkt")
(provide #%datum #%app #%top #%top-interaction)

(define-syntax-rule (qkstack-module-begin tree)
  (#%module-begin
   tree))
(provide (rename-out [qkstack-module-begin #%module-begin]))

(begin-for-syntax
  (define (top-level-form? stx)
    (syntax-case stx (%%top-level-form)
      [(%%top-level-form _) #t]
      [_ #f]))

  (define (operator? stx)
    (not (top-level-form? stx)))

  (define (qkstack->racket stx stack)
    (for/fold ([acc stack])
              ([expr (filter operator? (syntax->list stx))])
      #`(#,expr #,acc))))

(define-syntax (%%qkstack stx)
  (syntax-case stx ()
    [(_ operator ...)
     #`(begin
         #,@(filter top-level-form? (syntax->list stx))
         (module+ main
           (current-print (lambda (_) (void)))
           #,(qkstack->racket #'(operator ...)
                              #'(make-stack))))]))
(provide %%qkstack)

(define-syntax (block stx)
  (syntax-case stx ()
    [(_ operator ...)
     #`(lambda (stack)
         #,(qkstack->racket #'(operator ...) #'stack))]))

(define-syntax-rule (%%operator operator)
  operator)
(provide %%operator)

(define-syntax %%if
  (syntax-rules ()
    [(_ "(" "if" operator ")")
     (lambda (stack)
       (if (pop! stack)
           (operator stack)
           stack))]
    [(_ "(" "if" then-expr else-expr ")")
     (lambda (stack)
       (if (pop! stack)
           (then-expr stack)
           (else-expr stack)))]))
(provide %%if)

(define-syntax-rule (%%define "(" "define" name operator ... ")")
  (define name (block operator ...)))
(provide %%define)

(define (datum->word dat)
  (lambda (stack) (push! stack dat) stack))

(define (word-or-datum->word x)
  (if (procedure? x)
      x
      (datum->word x)))

(define-syntax (%%begin stx)
  (syntax-case stx (%%bindings)
    [(_ "(" "begin" operator ... ")")
     #`(lambda (stack)
         ((block operator ...) stack))]))
(provide %%begin)

(define-syntax (%%let stx)
  (syntax-case stx (%%bindings)
    [(_ "(" "let" (%%bindings "(" arg ... ")" ) operator ... ")")
     #`(lambda (stack)
         (let* #,(reverse (syntax->list #'([arg (word-or-datum->word (pop! stack))] ...)))
           ((block operator ...) stack)))]))
(provide %%let)

(define-syntax (%%named-let stx)
  (syntax-case stx (%%bindings)
    [(_ "(" "let" name (%%bindings "(" arg ... ")") operator ... ")")
     #`(lambda (stack)
         (define (name stack)
           (let* #,(reverse (syntax->list #'([arg (word-or-datum->word (pop! stack))] ...)))
             ((block operator ...) stack)))
         (name stack))]))
(provide %%named-let)

(define-syntax-rule (%%let-cc "(" "let/cc" name operator ... ")")
  (lambda (stack)
    (let/cc k
      (define (name stack2)
        (push! stack (pop! stack2))
        (k stack))
      ((block operator ...) stack))))
(provide %%let-cc)

(define-syntax-rule (%%top-level-form top-level-from)
  top-level-from)
(provide %%top-level-form)

(define-syntax-rule (%%require "(" "require" id-or-string ... ")")
  (require id-or-string ...))
(provide %%require)

(define-syntax-rule (%%provide "(" "provide" id ... ")")
  (provide id ...))
(provide %%provide)

(define-syntax %%datum
  (syntax-rules ()
    [(_ datum)
     (lambda (stack) (push! stack datum) stack)]
    [(_ "'" sexp)
     (%%datum sexp)]))
(provide %%datum)

(define-syntax %%sexp
  (syntax-rules ()
    [(_ datum) 'datum]
    [(_ "(" sexp ... ")")
     `(,sexp ...)]))
(provide %%sexp)

(define-syntax-rule (%%word word) word)
(provide %%word)

(define-syntax-rule (%%quote "," operator)
  (lambda (stack)
    (push! stack operator)
    stack))
(provide %%quote)
