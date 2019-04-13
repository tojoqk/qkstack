#lang qkstack
(require qkstack/prelude)

(define dup2 [a b -> a b a b]
  over over)

(define mod0? [a b -> bool]
  modulo zero?)

(define fizzbuzz [a b ->]
  dup2 = not
  (if (begin
        (let/cc continue
          dup 15 mod0?
          (if (begin "FizzBuzz" displayln continue))

          dup 3 mod0?
          (if (begin "Fizz" displayln continue))

          dup 5 mod0?
          (if (begin "Buzz" displayln continue))

          dup displayln)
        add1 fizzbuzz)))
100 1 fizzbuzz
