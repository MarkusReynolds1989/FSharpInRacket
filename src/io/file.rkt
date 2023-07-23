#lang racket/base
(require racket/file)

;;; This only works for text files.
(define (read-all-lines file-path)
  (file->lines file-path))

(define (read-all-bytes file-path)
  (file->bytes file-path))

(define (read-all-text file-path)
  (file->string file-path))

(module+ test
  (require rackunit)
  (define simple_multi_line "simple_multi_line.txt")

  (test-equal? "read-all-lines test" (list "1" "2" "3" "4") (read-all-lines simple_multi_line)))

  ; Git changes the line endings on upload so this will fail.
  ;(test-equal? "read-all-bytes test" #"1\r\n2\r\n3\r\n4\r\n" (read-all-bytes simple_multi_line))

  ; Git changes the line endings on upload so this will fail.
  ;(test-equal? "read-all-text test" "1\r\n2\r\n3\r\n4\r\n" (read-all-text simple_multi_line)))
