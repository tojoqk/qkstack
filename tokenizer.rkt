#lang racket/base
(require brag/support)

(module+ test
  (require rackunit))

(define-lex-abbrevs
  [whitespace (:+ (char-set " \t\r\n"))]
  [id-char (:~ (char-set " \t\r\n@()[]{}\",'`;#|\\"))]
  [id (:& (:- (:: (:- id-char digit) (:* id-char))
              (:: "%" (:* id-char))))]
  [digit (:/ #\0 #\9)])

(define (reserved? id)
  (or (string=? id "require")
      (string=? id "provide")
      (string=? id "define")
      (string=? id "if")
      (string=? id "begin")
      (string=? id "let/cc")))

(define (make-tokenizer in)
  (port-count-lines! in)
  (define qkstack-lexer
    (lexer-src-pos
     [whitespace (token lexeme #:skip? #t)]
     [(from/to ";" "\n") (token lexeme #:skip? #t)]
     ["(" (token 'LP lexeme)]
     [")" (token 'RP lexeme)]
     ["[" (token 'LB lexeme)]
     ["]" (token 'RB lexeme)]
     [id
      (if (reserved? lexeme)
          (token lexeme lexeme)
          (token 'ID (string->symbol lexeme)))]
     [(:+ digit)
      (token 'NUMBER (string->number lexeme))]
     [(from/to "\"" "\"")
      (token 'STRING (trim-ends "\"" lexeme "\""))]
     ["#t" (token 'TRUE #t)]
     ["#f" (token 'FALSE #f)]
     [(eof) (void)]))
  (lambda () (qkstack-lexer in)))
(provide make-tokenizer)

(module+ test
  (define (tokenize1 s)
    ((make-tokenizer (open-input-string s))))

  (check-equal? (tokenize1 "  (require")
                (position-token
                 (token-struct '|  | #f #f #f #f #f #t)
                 (position 1 1 0)
                 (position 3 1 2)))

  (check-equal? (tokenize1 "(require")
                (position-token
                 (token-struct 'LP "(" #f #f #f #f #f)
                 (position 1 1 0)
                 (position 2 1 1)))

  (check-equal? (tokenize1 "require")
                (position-token
                 (token-struct
                  'require "require" #f #f #f #f #f)
                 (position 1 1 0)
                 (position 8 1 7)))

  (check-equal? (tokenize1 "id")
                (position-token
                 (token-struct 'ID 'id #f #f #f #f #f)
                 (position 1 1 0)
                 (position 3 1 2)))

  (check-exn #rx"lexer:" (lambda () (tokenize1 "%id"))))
