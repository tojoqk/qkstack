#lang racket
(provide #%datum #%app #%top #%top-interaction)

(define-syntax-rule (qkstack-module-begin tree)
  (#%module-begin
   (write 'tree)))
(provide (rename-out [qkstack-module-begin #%module-begin]))
