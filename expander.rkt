#lang racket
(require "stack.rkt")
(provide #%datum #%app #%top #%top-interaction)

(define-syntax-rule (qkstack-module-begin tree)
  (#%module-begin
   tree))
(provide (rename-out [qkstack-module-begin #%module-begin]))

(define current-operator-stack (make-parameter (make-stack)))

(begin-for-syntax
  (define (require? expr)
    (eq? '%%require (car (syntax->datum expr)))
    (syntax-case expr (%%expression %%form %%require)
      [(%%expression (%%form "(" (%%require _ ...) ")")) #t]
      [_ #f]))
  (define (define? expr)
    (syntax-case expr (%%expression %%form %%define)
      [(%%expression (%%form "(" (%%define _ ...) ")")) #t]
      [_ #f]))

  (define (provide? expr)
    (syntax-case expr (%%expression %%form %%provide)
      [(%%expression (%%form "(" (%%provide _ ...) ")")) #t]
      [_ #f]))

  (define (init-expressions stx)
    (let ([exprs (syntax->list stx)])
      (append (filter require? exprs)
              (filter define? exprs)
              (filter provide? exprs))))

  (define (apply-expressions stx stack)
    (let ([exprs
           (filter (lambda (expr)
                     (not (or (require? expr)
                              (define? expr)
                              (provide? expr))))
                   (syntax->list stx))])
      (for/fold ([acc stack])
                ([expr exprs])
        #`(#,expr #,acc)))))

(define-syntax (%%qkstack stx)
  (syntax-case stx ()
    [(_ expression ...)
     #`(begin
         #,@(init-expressions #'(expression ...))
         #,(apply-expressions #'(expression ...) #'(make-stack)))]))
(provide %%qkstack)

(define-syntax (%%block stx)
  (syntax-case stx ()
    [(_ "[" expression ... "]")
     #`(lambda (stack)
         #,@(init-expressions #'(expression ...))
         #,(apply-expressions #'(expression ...) #'stack))]))
(provide %%block)

(define-syntax %%expression
  (syntax-rules (%%block)
    [(_ (%%block body ...))
     (current-operator-stack
      ((%%block body ...) (current-operator-stack)))]
    [(_ expression)
     expression]))
(provide %%expression)

(define-syntax-rule (%%form "(" form ")")
  form)
(provide %%form)

(define-syntax %%if
  (syntax-rules ()
    [(_ "if" block)
     (lambda (stack)
       (if (pop! stack)
           (block stack)
           stack))]
    [(_ "if" then-block else-block)
     (lambda (stack)
       (if (pop! stack)
           (then-block stack)
           (else-block stack)))]))
(provide %%if)

(define-syntax-rule (%%define "define" name block)
  (define name block))
(provide %%define)

(begin-for-syntax
  (define (strip-%%sexp sexp)
    (cond
      [(and (pair? sexp)
            (eq? '%%sexp (car sexp)))
       (strip-%%sexp (cadr sexp))]
      [else sexp])))

(define-syntax (%%require stx)
  (syntax-case stx ()
    ([k "require" sexp ...]
     #`(require #,@(datum->syntax
                    #'k
                    (map strip-%%sexp
                         (syntax->datum #'(sexp ...))))))))
(provide %%require)

(define-syntax (%%provide stx)
  (syntax-case stx ()
    ([k "provide" sexp ...]
     #`(provide #,@(datum->syntax
                    #'k
                    (map strip-%%sexp
                         (syntax->datum #'(sexp ...))))))))
(provide %%provide)

(define-syntax %%datum
  (syntax-rules ()
    [(_ datum)
     (lambda (stack)
       (push! stack datum)
       stack)]
    [(_ "'" sexp)
     (%%datum sexp)]))
(provide %%datum)

(define-syntax-rule (%%word name)
  name)
(provide %%word)

(define-syntax %%sexp
  (syntax-rules ()
    [(_ sexp) 'sexp]
    [(_ "'" sexp)
     `(quote ,sexp)]
    [(_ "(" sexp ... ")")
     `(,sexp ...)]))
(provide %%sexp)
