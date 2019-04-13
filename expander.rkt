#lang racket/base
(require (for-syntax racket/base)
         (for-syntax racket/contract)
         "stack.rkt")
(provide #%datum #%app #%top
         #%top-interaction
         #%module-begin)

(begin-for-syntax
  (define/contract (operator? stx)
    (-> syntax? boolean?)
    (syntax-case stx (%operator)
      [(%operator _) #t]
      [_ #f]))

  (define/contract (form? stx)
    (-> syntax? boolean?)
    (not (operator? stx))))

(define-syntax (%qkstack stx)
  (syntax-case stx ()
    [(_ op-or-form ...)
     (with-syntax ([(operator ...)
                    (filter operator?
                            (syntax->list
                             #'(op-or-form ...)))]
                   [(form ...)
                    (filter form?
                            (syntax->list
                             #'(op-or-form ...)))])
       #`(begin
           form ...
           (module+ main
             (let ([stack (make-stack)])
               (operator stack)
               ...))))]))
(provide %qkstack)

(define-syntax-rule (%form form)
  form)
(provide %form)

(define-syntax-rule (%require "(" "require" id ... ")")
  (require id ...))
(provide %require)

(define-syntax-rule (%provide "(" "provide" id ... ")")
  (provide id ...))
(provide %provide)

(define-syntax-rule (%operator operator)
  operator)
(provide %operator)

(define-syntax-rule (%word word) word)
(provide %word)

(define-syntax-rule (%datum datum)
  (lambda (stack) (push! stack datum)))
(provide %datum)

(define-syntax %if
  (syntax-rules ()
    [(_ "(" "if" operator ")")
     (lambda (stack)
       (if (pop! stack)
           (operator stack)
           (void)))]
    [(_ "(" "if" then-op else-op ")")
     (lambda (stack)
       (if (pop! stack)
           (then-op stack)
           (else-op stack)))]))
(provide %if)

(define-syntax-rule (block operator ...)
  (lambda (stack)
    (operator stack) ...))

(define-syntax %define
  (syntax-rules (%comment)
    [(_ "(" "define" id (%comment _ ...)
        operator ...
        ")")
     (%define "(" "define" id operator ... ")")]
    [(_ "(" "define" id operator ... ")")
     (define id
       (block operator ...))]))
(provide %define)

(define-syntax-rule (%begin "(" "begin" operator ... ")")
  (block operator ...))
(provide %begin)

(define-syntax-rule (%let-cc "(" "let/cc" name operator ... ")")
  (lambda (stack)
    (let/cc name
      (operator stack)
      ...)))
(provide %let-cc)
