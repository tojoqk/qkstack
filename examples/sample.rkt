#lang qkstack
(require qkstack/prelude)

"hello"
displayln

(define factorial
  dup zero?
  (if [then drop 1]
      [else dup sub1 factorial *]))
10 factorial

(define append
  over null? (if [then nip]
                 [else over cdr swap append swap car swap cons]))
'(a b c) '(1 2 3) append

10
(let fact ()
  dup zero? (if [then drop 1]
                [else dup sub1 fact *]))

'(1 2 10 3 4 5)
(let product ()
  dup null?
  (if [then drop 1]
      [else dup car swap cdr product *]))

(define product1
  dump displayln
  dup null?
  (if [then drop 1]
      [else dup car swap cdr product1 *]))

(define product2
  (let/cc escape
    (let loop ()
      dump displayln
      dup null?
      (if [then drop 1]
          [else
           dup car zero? (if [then drop 0 escape])
           dup car swap cdr loop *])))
  nip)

'(1 2 0 3 4 5)
dup
product1
swap
product2

,product2 (let (f) '(10 20 30) f)
