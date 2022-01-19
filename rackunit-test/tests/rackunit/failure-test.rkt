#lang racket/base

(require rackunit
         rackunit/text-ui
         racket/port
         tests/eli-tester)

(define output
  (with-output-to-string
    (lambda ()
      (parameterize ([current-error-port (current-output-port)])
        (run-tests (test-suite "tests"
                     (check-equal? 1 4)
                     (check-equal? 1 1)))))))

(test
 (regexp-match
  (regexp (regexp-quote "1 success(es) 1 failure(s) 0 error(s) 2 test(s) run\n"))
  output))

(module test racket/base
  (require syntax/location)
  ;; Use a separate namespace to avoid logging results
  ;; in this namespace (where `raco test` would see errors).
  (parameterize ([current-namespace (make-base-namespace)])
    (dynamic-require (quote-module-path "..") #f)))
