#lang racket/base
(require syntax/strip-context
         "parser.rkt" "tokenizer.rkt")

(module+ reader
  (provide read-syntax))

(define (read-syntax path port)
  (define syntax-tree (parse path (make-tokenizer port path)))
  (strip-context
   #`(module sexp-drill-mod qkstack/expander
       #,syntax-tree)))
