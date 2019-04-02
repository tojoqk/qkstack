#lang racket
(require "stack.rkt")
(provide #%datum #%app #%top #%top-interaction)

(define-syntax-rule (qkstack-module-begin tree)
  (#%module-begin
   tree))
(provide (rename-out [qkstack-module-begin #%module-begin]))

(define current-operator-stack (make-parameter (make-stack)))
(define (push-operator! op)
  (let ([op-stack (current-operator-stack)])
    (push! op-stack op)
    (current-operator-stack op-stack)))

(define (run-operator-stack op-stack)
  (let run ([data-stack (make-stack)]
            [op-stack op-stack])
    (if (stack-empty? op-stack)
        data-stack
        (let ([op (pop! op-stack)])
          (let-values ([(data-stack op-stack) (op data-stack op-stack)])
            (run data-stack op-stack))))))

(define-syntax (%%qkstack stx)
  (syntax-case stx ()
    [(_ expression ...)
     (with-syntax ([(rev-expr ...)
                    (reverse (syntax->list #'(expression ...)))])
       #'(begin
           rev-expr ...
           (run-operator-stack (current-operator-stack))))]))
(provide %%qkstack)

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
     (push-operator!
      (lambda (data-stack op-stack)
        (if (pop! data-stack)
            (values data-stack (block op-stack))
            (values data-stack op-stack))))]
    [(_ "if" then-block else-block)
     (push-operator!
      (lambda (data-stack op-stack)
        (if (pop! data-stack)
            (values data-stack (then-block op-stack))
            (values data-stack (else-block op-stack)))))]))
(provide %%if)

(define-syntax-rule (%%define "define" name block)
  (define name
    (lambda (data-stack op-stack)
      (values data-stack (block op-stack)))))
(provide %%define)

(define-syntax (%%block stx)
  (syntax-case stx ()
    [(_ "[" expression ... "]")
     (with-syntax ([(rev-expr ...)
                    (reverse (syntax->list #'(expression ...)))])
       #'(lambda (op-stack)
           (parameterize ([current-operator-stack op-stack])
             rev-expr ...
             (current-operator-stack))))]))
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
      (lambda (data-stack op-stack)
        (push! data-stack datum)
        (values data-stack op-stack)))]
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
