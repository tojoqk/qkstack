#lang qkstack
(require qkstack/prelude)

;; Hello, World!
"Hello, World!" displayln

(define print
  display " -> " display displayln)

3 1 2 + 10 * -
"3 1 2 + 10 * -"
print

;; Write factorial function
(define factorial
  dup zero?
  (if (begin drop 1)
      (begin dup sub1 factorial *)))
(provide factorial)

10 factorial
"10 factorial"
print

;; list processing
(define append
  over null?
  (if (begin
        swap drop)
      (begin
        over car rot cdr rot append cons)))

'(a b c) '(1 2 3) append
"'(a b c) '(1 2 3) append"
print

;; lexical binding
4 2 (let (a b) a b * a b + -)
"4 2 (let (a b) a b * a b + -)"
print

;; lexical binding and word quoting
,+ (let (op) 22 20 op)
",+ (let (op) 22 20 op)"
print

(define seq
  dup zero?
  (if drop
      (begin dup sub1 seq)))

10 seq dump
"10 seq dump"
print

(define dotimes
  (let loop (n block)
    n zero? not
    (if (begin block n sub1 ,block loop))))
,+ 9 dotimes
",+ 9 dotimes"
print

(define product
  over 'end eq?
  (if nip
      (begin * product)))
'end 1 2 3 4 5 product
"'end 1 2 10 4 5 product"
print

(define reduce
  (let (op mark)
    (let loop ()
      over mark eq?
      (if nip
          (begin op loop)))))
'end 10 seq ,* 'end reduce
"'end 10 seq ,* 'end reduce"
print
