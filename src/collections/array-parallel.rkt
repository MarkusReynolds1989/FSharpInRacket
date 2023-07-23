#lang racket/base
(require "../globals.rkt"
         racket/future
         (prefix-in Array. "Array.rkt"))

; Tests if any element of the array satisifes the given predicate.
(define (Parallel.exists predicate input)
  (define predicates
    (for/vector ([elem (in-vector input)])
      (future (fn () (predicate elem)))))

  (for/or ([item (in-vector predicates)])
    (touch item)))

(define (Parallel.map mapping input)
  (define mappings
    (for/vector ([elem (in-vector input)])
      (future (fn () (mapping elem)))))

  (for/vector ([elem (in-vector mappings)])
    (touch elem)))

(provide (all-defined-out))

(module+ test
  (require rackunit)
  (test-true "Exists works." (Parallel.exists (fn (x) (= x 1)) #(4 3 2 1)))
  (test-false "Exists works with no found value."
              (Parallel.exists (fn (x) (= x 1)) #(100 200 300 44)))
  (test-equal? "Map works." (Parallel.map (fn (x) (+ x 1)) #(1 2 3 4)) #(2 3 4 5)))

#| (test-case "Giant map test."
    (define start-time (current-inexact-milliseconds))
    (check-equal? (Parallel.map (fn (x) (+ x 1)) (Array.create 1000000 1)) (Array.create 1000000 2))
    (define end-time (current-inexact-milliseconds))
    (define elapsed-time (- end-time start-time))
    (displayln elapsed-time)))|#
