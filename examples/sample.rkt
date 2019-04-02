#lang qkstack
(require qkstack/prelude)

(define factorial
  [dup zero? (if [drop 1]
                 [dup sub1 factorial *])])
[10 factorial]

(define append
  [over null? (if [nip]
                  [over cdr swap append swap car swap cons])])

'(a b c) '(1 2 3) append
