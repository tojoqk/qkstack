#lang racket
(require brag/support)

(define (make-tokenizer in path)
  (port-count-lines! in)
  (lexer-file-path path)
  (define qkstack-lexer
    (lexer-src-pos
     [whitespace (token lexeme #:skip? #t)]
     [(from/to ";" "\n") (token 'COMMENT #:skip? #t)]
     [(:or "(" ")" "'" ",") (token lexeme lexeme)]
     [(:or "if" "begin"
           "define" "let" "let/cc"
           "require" "provide")
      (token lexeme lexeme)]
     [(:+ (char-set "0123456789"))
      (token 'NUMBER (string->number lexeme))]
     [(:+ (:~ (char-set " \n@()[]{}\",'`;#|\\")))
      (token 'IDENTIFIER (string->symbol lexeme))]
     [(from/to "\"" "\"")
      (token 'STRING (trim-ends "\"" lexeme "\""))]
     ["#t" (token 'TRUE #t)]
     ["#f" (token 'FALSE #f)]
     [(eof) (void)]))
  (thunk (qkstack-lexer in)))
(provide make-tokenizer)
