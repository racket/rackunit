#lang racket/base
(require rackunit
         rackunit/text-ui
         racket/port
         racket/match
         racket/system
         racket/runtime-path)

(define-runtime-path me "13.rkt")

(define a
  (test-suite "Test Suite"
              (test-case "Test Case"
                (check-equal? #t #f))))

(define b
  (test-suite "Test Suite"
              (test-case "Test Case"
                (check-equal? (error 'error "I'm an error!") #f))))

(module+ test
  (define mode (getenv "PR13"))
  (printf "\n\nRunning in mode ~v\n\n" mode)
  (match mode
    ["a" (run-tests a)]
    ["a-raw" (check-equal? #t #f)]
    ["b" (run-tests b)]
    ["b-raw" (check-equal? (error 'error "I'm an error!") #f)]
    [#f
     (for ([v (in-list '("a" "a-raw" "b" "b-raw"))])
       (putenv "PR13" v)
       (printf "Readying mode ~v\n" v)
       (check-equal?
        (parameterize ([current-output-port (open-output-nowhere)])
          (parameterize ([current-error-port (current-output-port)])
            (system/exit-code (format "raco test ~a" me))))
        1))]))
