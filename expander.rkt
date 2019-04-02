#lang racket
(require "stack.rkt")
(provide #%datum #%app #%top #%top-interaction)

(define-syntax-rule (qkstack-module-begin tree)
  (#%module-begin
   tree))
(provide (rename-out [qkstack-module-begin #%module-begin]))

(define current-operators (make-parameter '()))

(define (push-operator! operator)
  (current-operators
   (cons operator (current-operators))))

(define (eval-current-operators stack)
  (for/fold ([stack stack])
            ([operator (reverse (current-operators))])
    (operator stack)))

(define-syntax-rule (%%qkstack expression ...)
  (begin
    expression ...
    (eval-current-operators (make-stack))))
(provide %%qkstack)

(define-syntax %%expression
  (syntax-rules (%%block)
    [(_ (%%block body ...))
     (push-operator!
      (%%block body ...))]
    [(_ expression)
     expression]))
(provide %%expression)

(define-syntax-rule (%%form "(" form ")")
  form)
(provide %%form)

(define-syntax %%if
  (syntax-rules ()
    [(_ "if" block)
     (push-operator!
      (lambda (stack)
        (if (pop! stack)
            (block stack)
            stack)))]
    [(_ "if" then-block else-block)
     (push-operator!
      (lambda (stack)
        (if (pop! stack)
            (then-block stack)
            (else-block stack))))]))
(provide %%if)

(define-syntax-rule (%%define "define" name block)
  (define name block))
(provide %%define)

(define-syntax-rule (%%block "[" expression ... "]")
  (lambda (stack)
    (parameterize ([current-operators '()])
      expression ...
      (eval-current-operators stack))))
(provide %%block)

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
     (push-operator!
      (lambda (stack)
        (push! stack datum)
        stack))]
    [(_ "'" sexp)
     (%%datum sexp)]))
(provide %%datum)

(define-syntax-rule (%%word name)
  (push-operator! name))
(provide %%word)

(define-syntax %%sexp
  (syntax-rules ()
    [(_ sexp) 'sexp]
    [(_ "'" sexp)
     `(quote ,sexp)]
    [(_ "(" sexp ... ")")
     `(,sexp ...)]))
(provide %%sexp)
