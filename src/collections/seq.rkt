#lang racket/base
(require racket/stream
         "../globals.rkt")

(define (append seq-one seq-two)
  (stream-append seq-one seq-two))

(define (create . values)
  (stream* values))

(define (fold function accumulator sequence)
  (stream-fold function accumulator sequence))

(define (head sequence)
  (stream-first sequence))

(define (map function sequence)
  (stream-map function sequence))

(provide all-defined-out)

(module+ test
  (require rackunit
           threading)

  (check-eq? 1 (head (create 1 2 3)))
  (check-equal? (stream->list (stream 1 2 3)) (stream->list (create 1 2 3)))
  (check-equal? (stream->list (create 1 2 3 4 5)) (stream->list (append (create 1 2) (create 3 4 5))))
  (check-equal? (stream->list (create 3 4 5))
                (stream->list (~>> (create 2 3 4) (map (fn (x) (+ x 1)))))))