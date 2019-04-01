#lang racket
(require brag/support)

(define (make-tokenizer in path)
  (port-count-lines! in)
  (lexer-file-path path)
  (define qkstack-lexer
    (lexer-src-pos
     [whitespace (token lexeme #:skip? #t)]
     ["if{" (token 'IF-BEGIN lexeme)]
     ["}else{" (token 'IF-ELSE lexeme)]
     ["{" (token 'BEGIN lexeme)]
     ["}" (token 'END lexeme)]
     [":" (token 'DEFINE-BEGIN lexeme)]
     [(:+ (char-set "0123456789"))
      (token 'NUMBER (string->number lexeme))]
     [(:+ (:~ (char-set " \n@()[]{}\",'`;#|\\")))
      (token 'IDENTIFIER (string->symbol lexeme))]
     ["(" (token 'LEFT-PARENCE lexeme)]
     [")" (token 'RIGHT-PARENCE lexeme)]
     [(from/to "\"" "\"")
      (token 'STRING (trim-ends "\"" lexeme "\""))]
     [(eof) (void)]))
  (thunk (qkstack-lexer in)))
(provide make-tokenizer)
