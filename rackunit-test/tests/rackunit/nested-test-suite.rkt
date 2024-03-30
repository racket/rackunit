#lang racket/base

(module core racket/base
  (require rackunit
           rackunit/text-ui)
  (run-tests (test-suite "tests"
               (test-suite "sub-tests"
                 (check-equal? 1 2)))))

(module test racket/base
  (require syntax/location
           racket/port
           rackunit)
  ;; Use a separate namespace to avoid logging results
  ;; in this namespace (where `raco test` would see errors).
  (define output
    (with-output-to-string
      (lambda ()
        (parameterize ([current-error-port (current-output-port)]
                       [current-namespace (make-base-namespace)])
          (dynamic-require (quote-module-path ".." core) #f)))))
  (check-regexp-match
   (regexp (regexp-quote "--------------------\ntests > sub-tests > Unnamed test\nFAILURE"))
   output))
