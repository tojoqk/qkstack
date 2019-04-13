#lang qkstack
(require qkstack/prelude)

(define dup2 [ a b -> a b a b ]
  over over)

(define mod0? [ a b -> bool ]
  modulo zero?)

(define fizzbuzz [ a b -> ]
  dup2 = not
  (if (begin
        (let/cc continue
          over 15 mod0?
          (if (begin "FizzBuzz" displayln continue))

          over 3 mod0?
          (if (begin "Fizz" displayln continue))

          over 5 mod0?
          (if (begin "Buzz" displayln continue))

          over displayln)
        swap add1 swap fizzbuzz)))
1 100 fizzbuzz
