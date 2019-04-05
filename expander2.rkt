#lang racket
(require "stack.rkt")
(provide #%datum #%app #%top #%top-interaction)

(define-syntax-rule (qkstack-module-begin tree)
  (#%module-begin
   tree))
(provide (rename-out [qkstack-module-begin #%module-begin]))

(define current-operator-stack
  (make-parameter (make-stack)))

(define (op-push! op)
  (push! (current-operator-stack) op))

(define (op-pop!)
  (pop! (current-operator-stack)))

(define (op-empty?)
  (stack-empty? (current-operator-stack)))

(begin-for-syntax
  (define (top-level-form? stx)
    (syntax-case stx (%%top-level-form)
      [(%%top-level-form _) #t]
      [_ #f]))

  (define (operator? stx)
    (not (top-level-form? stx))))

(define-syntax (%%qkstack stx)
  (syntax-case stx ()
    [(_ form ...)
     (with-syntax ([(top-level-form ...)
                    (filter top-level-form?
                            (syntax->list #'(form ...)))]
                   [(operator ...)
                    (reverse
                     (filter operator?
                             (syntax->list #'(form ...))))])
       #`(begin
           top-level-form ...
           (module+ main
             (current-print (lambda (_) (void)))
             (op-push! operator) ...
             (let ([data-stack (make-stack)])
               (let loop ()
                 (unless (op-empty?)
                   ((op-pop!) data-stack)
                   (loop)))))))]))
(provide %%qkstack)

(define-syntax (block stx)
  (syntax-case stx ()
    [(_ operator ...)
     #`(lambda (stack)
         #,@(reverse
             (syntax->list #'((op-push! operator) ...))))]))

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
    [(_ "(" "if" then-op else-op ")")
     (lambda (stack)
       (if (pop! stack)
           (then-op stack)
           (else-op stack)))]))
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
     #`(block operator ...)]))
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
    (let ([saved (dump (current-operator-stack))])
      (define (name stack)
        (load! (current-operator-stack) saved)
        stack)
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
