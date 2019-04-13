#lang racket/base
(require syntax/strip-context
         "parser.rkt" "tokenizer.rkt")


(define (read-syntax path port)
  (define stx
    (parameterize ([lexer-file-path path])
      (parse path (make-tokenizer port))))
  (strip-context
   #`(module qkstack-mod qkstack/expander
       #,stx)))

(module+ reader
  (provide read-syntax))
