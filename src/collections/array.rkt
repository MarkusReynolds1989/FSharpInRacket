#lang racket/base

; TODO: array-all-pairs

(define (append array-one array-two)
  (let ([new-array (make-vector (+ (length array-one) (vector-length array-two)) (get 0 array-one))])
    (vector-copy! new-array 0 array-one 0 (length array-one))
    (vector-copy! new-array (length array-one) array-two 0 (vector-length array-two))
    new-array))

; Come back after sum.
;(: array-average (-> (Vectorof Number) Number))
;(define (array-average source)
;  )

; TODO: array-average-by

; TODO: array-blit, this is just vector-copy!

; TODO: array-choose

; TODO: array-chunk-by-size

; TODO: array-collect - implement map and concat

; TODO: array-compare-with

; TODO: array-concat

(define (contains value array)
  (let ([result #f])
    (for ([index (in-range 0 (length array))])
      (when (equal? value (get index array))
        (set! result #t)))
    result))

(define (copy array)
  (define new-array (create (length array) (get 0 array)))
  (vector-copy! new-array 0 array 0 (length array))

  new-array)

(define (copy-to in-array out-array)
  (vector-copy! out-array 0 in-array 0 (length out-array)))

; TODO: array-count-by

(define (create count init-value)
  (make-vector count init-value))

; TODO: array-distinct - just a hashmap or set if they have it.

; TODO: array-distinct-by

(define (empty)
  (vector))

(define (exactly-one array)
  (and (= (length array) 1) (get 0 array)))

; TODO: array-except

(define (exists predicate array)
  (define result #f)
  (for ([i (in-range 0 (length array))])
    (when (predicate (get i array))
      (set! result #t)))

  result)

; TODO: array-exists-two

; TODO: array-fill target target-index count value

(define (filter predicate array)
  (fold (lambda (acc item) (if (predicate item) (append (vector item) acc) acc))
        (create 0 (get 0 array))
        array))

(define (fold folder state array)
  (let loop ([index 0] [folder folder] [state state] [array array])
    (cond
      [(= index (length array)) state]
      [else (loop (+ 1 index) folder (folder state (get index array)) array)])))

; TODO: array-fold-two

; TODO: array-fold-back

; TODO: array-fold-back-two

(define (for-all predicate array)
  (let loop ([index 0] [predicate predicate] [array array])
    (cond
      [(= index (length array)) #t]
      [(not (predicate (get index array))) #f]
      [else (loop (+ index 1) predicate array)])))

; TODO: array-for-all-two

(define (get index array)
  (vector-ref array index))

; TODO: array-group-by

(define (head array)
  (get 0 array))

(define (init count
              initializer)
  (for/vector ([index (in-range 0 count)])
    (initializer index)))

; TODO: array-insert-at

; TODO: array-insert-many-at

; TODO: array-is-empty?
(define (is-empty? array)
  (= (length array) 0))

; TODO: array-item is the same as array-get, may not do it.

(define (iter action array)
  (for ([index (in-range 0 (length array))])
    (action (get index array))))

(define (iter-two action array-one array-two)
  (for ([index (in-range 0 (length array-one))])
    (action (get index array-one) (get index array-two))))

(define (iter-index action array)
  (for ([index (in-range 0 (length array))])
    (action index (get index array))))

(define (iter-index-two action array-one array-two)
  (for ([index (in-range 0 (length array-one))])
    (action index (get index array-one) (get index array-two))))

(define (last array)
  (get (- (length array) 1) array))

(define (length array)
  (vector-length array))

;Builds a new array whose elements are the results of applying the given function to each of the elements to the array.
(define (map mapper array)
  (for/vector ([item array])
    (mapper item)))

(define (sum array)
  (fold (lambda (acc item) (+ acc item)) 0 array))

;(define (sort array)
;  (vector-sort array))

(provide (all-defined-out))

(module+ test
  (require rackunit)
  (define old-array (vector 1 2 3 4))

  (check-equal? (append (vector 1 2 3) (vector 4 5 6)) (vector 1 2 3 4 5 6))
  (check-true (contains -100 (vector 1 2 3 4 100 23 -100)))
  (check-false (contains "blue" (vector "red" "yellow" "green")))
  (check-not-eq? old-array (copy old-array))
  (check-equal? (copy old-array) old-array)
  (check-equal? (create 3 0) (vector 0 0 0))
  (check-false (exactly-one (vector 1 2)))
  (check-eq? (exactly-one (vector 1)) 1)
  (check-true (exists (lambda (item) (= item 1)) (vector 1 2 3 4)))
  (check-false (exists (lambda (item) (equal? item "blue")) (vector "red" "yellow" "green")))

  (check-eq? (fold (lambda (acc item) (+ acc item)) 0 (vector 1 2 3 4)) 10)

  (check-equal? (filter (lambda (item) (= item 1)) (vector 1 1 1 2 2 2)) (vector 1 1 1))

  (check-equal? (filter (lambda (item) (> (string-length item) 3)) (vector "one" "two" "three"))
                (vector "three"))

  (check-true (for-all (lambda (item) (> item 2)) (vector 3 4 5 6 7)))
  (check-false (for-all (lambda (item) (> item 2)) (vector 1 2 3 4 5)))
  (check-eq? (get 0 (vector 1 2 3 4)) 1)
  (check-equal? (map (lambda (item) (add1 item)) (vector 1 2 3 4)) (vector 2 3 4 5))
  (check-equal? (map (lambda (item) (string-length item)) (vector "one" "two" "three"))
                (vector 3 3 5))

  (check-equal? (init 3
                      (lambda (item) (add1 item)))
                (vector 1 2 3))

  (check-true (is-empty? (vector)))
  (check-false (is-empty? (vector 1)))

  (check-eq? (last (vector 1 2 3 4)) 4))