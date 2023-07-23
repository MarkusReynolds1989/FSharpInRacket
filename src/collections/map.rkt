#lang racket/base

(require (prefix-in List. "../collections/list.rkt")
         "../globals.rkt")

(define (add key value table)
  (hash-set table key value))

(define (change key value table)
  (hash-set table key value))

(define (contains-key key table)
  (hash-has-key? table key))

(define (count table)
  (hash-count table))

(define (empty)
  (hash))

(define (exists predicate table)
  (> (count (filter predicate table)) 0))

(define (get key table)
  (hash-ref table key))

(define (keys table)
  (hash-keys table))

(define (map mapping table)
  (hash-map/copy table mapping))

(define (map-filter predicate acc pair)
  (if (predicate pair) (add (car pair) (cdr pair) acc) acc))

(define (filter predicate table)
  (fold (fn (acc pair) (map-filter predicate acc pair)) (empty) table))

(define (find key table)
  (hash-ref table key))

(define (fold folder state table)
  (let loop ([index 0] [folder folder] [state state] [table table])
    (cond
      [(= index (count table)) state]
      [#t (loop (add1 index) folder (folder state (hash-iterate-pair table index)) table)])))

(define (to-list table)
  (hash-map table (fn (key value) (list key value))))

(define (to-array table)
  (List.to-array (hash-map table (fn (key value) (list key value)))))

(define (get-values table)
  (hash-values table))

(provide (all-defined-out)
         (except-out map-filter))

(module+ test
  (require rackunit)
  (define table (hash 1 "one"))
  (define add-table (hash "one" 1 "two" 2 "three" 3))

  (test-equal? "Add works." (add 2 "two" table) (hash 1 "one" 2 "two"))

  (test-equal? "Change works." (change 1 "three" table) (hash 1 "three"))

  (test-true "Contains works." (contains-key 1 table))

  (test-eq? "Count works." (count table) 1)

  (test-eq? "Get works." (get 1 table) "one")

  (test-equal? "Empty works." (empty) (hash))

  (test-equal? "To-list works." (to-list table) (list (list 1 "one")))

  (test-equal? "To-array works." (to-array table) (vector (list 1 "one")))

  (test-equal? "Map works." (map (fn (key value) (values (add1 key) value)) table) (hash 2 "one"))

  (test-equal? "Fold works." (fold (fn (acc pair) (+ (cdr pair) acc)) 0 add-table) 6)

  (test-equal? "Filter works."
               (filter (fn (pair) (> (cdr pair) 1)) add-table)
               (hash "two" 2 "three" 3))

  (test-true "Exists works." (exists (fn (x) (equal? (car x) "one")) add-table))

  (test-equal? "Find works." (find "one" add-table) 1))
