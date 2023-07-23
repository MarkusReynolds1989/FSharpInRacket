#lang racket/base

(define-syntax-rule (fn args body ...)
  (lambda args
    body ...))

(define min-int -217483648)
(define max-int 217483648)

(provide fn max-int min-int)
