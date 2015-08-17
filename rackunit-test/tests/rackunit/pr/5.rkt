#lang racket/base
(module+ test
  (require rackunit
           racket/port
           rackunit/text-ui)

  (define t (open-output-string))
  (parameterize ([current-error-port t])
    (run-tests
     (test-suite "x"
                 (check-not-false (displayln 0)))))
  (check-equal? (get-output-string t) ""))
