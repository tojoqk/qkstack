#lang qkstack
(require qkstack/prelude)

(define factorial [ n -> n! ]
  dup zero?
  (if (begin drop 1)
      (begin dup sub1 factorial *)))

5 factorial
displayln
