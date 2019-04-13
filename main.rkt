#lang racket/base
(require syntax/strip-context
         brag/support
         "parser.rkt" "tokenizer.rkt")

(module+ test
  (require rackunit))

(define (read-syntax path port)
  (define stx
    (parameterize ([lexer-file-path path])
      (parse path (make-tokenizer port))))
  (strip-context
   #`(module qkstack-mod qkstack/expander
       #,stx)))

(module+ reader
  (provide read-syntax))

(module+ test
  (define first-sample-code
    "(require qkstack/prelude)
     18 20 30 10 + 3 12 * - + +
     displayln")
  (check-equal?
   (parse-to-datum
    (make-tokenizer (open-input-string first-sample-code)))
   '(%qkstack
     (%form (%require "(" "require" qkstack/prelude ")"))
     (%operator (%datum 18))
     (%operator (%datum 20))
     (%operator (%datum 30))
     (%operator (%datum 10))
     (%operator (%word +))
     (%operator (%datum 3))
     (%operator (%datum 12))
     (%operator (%word *))
     (%operator (%word -))
     (%operator (%word +))
     (%operator (%word +))
     (%operator (%word displayln)))))
