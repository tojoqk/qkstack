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

10
(let fact ()
  [dup zero? (if [drop 1]
                 [dup sub1 fact *])])

'(1 2 10 3 4 5)
(let product ()
  [dup null?
       (if [drop 1]
           [dup car swap cdr product *])])


'(1 2 0 3 4 5)
(let/cc escape
  [(let product ()
     [
      dup null? (if [drop 1])
      dup car zero? (if [drop 0 escape])
      dup car swap cdr product *
      ])])
nip
