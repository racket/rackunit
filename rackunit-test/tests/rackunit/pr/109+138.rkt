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
                     (check-equal? 1 1)
                     (check-equal? (1) 1)
                     (check-equal? 1 2)
                     (check-equal? 1 1)))
        (run-tests (test-suite "tests2"
                     (check-equal? 5 5)
                     (check-equal? 4 4)
                     (check-equal? 3 3)
                     (check-equal? 2 2)
                     (check-equal? 1 1)))))))

(test
 (regexp-match
  (regexp (regexp-quote "2 success(es) 1 failure(s) 1 error(s) 4 test(s) run\n"))
  output))

(test
 (regexp-match
  (regexp (regexp-quote "5 success(es) 0 failure(s) 0 error(s) 5 test(s) run\n"))
  output))

(test
 (with-handlers ([exn:fail? (λ (e)
                              (regexp-match
                               (regexp (regexp-quote "given: 3"))
                               (exn-message e)))])
   (define my-check-equal? check-equal?)
   (run-tests (test-suite "tests"
                (my-check-equal? 1 1)
                (my-check-equal? (3) 1)
                (my-check-equal? 1 2)))
   #f))

(module test racket/base
  (require syntax/location)
  ;; Use a separate namespace to avoid logging results
  ;; in this namespace (where `raco test` would see errors).
  (parameterize ([current-namespace (make-base-namespace)])
    (dynamic-require (quote-module-path "..") #f)))
