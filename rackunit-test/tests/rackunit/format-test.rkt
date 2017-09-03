#lang racket/base

(require racket/function
         racket/port
         rackunit
         rackunit/private/check-info
         rackunit/private/format
         (submod rackunit/private/format for-test))

(define-check (check-output expected thnk)
  (define actual (with-output-to-string thnk))
  (with-check-info* (list (make-check-actual actual)
                          (make-check-expected expected))
    (thunk
     (unless (equal? actual expected)
       (fail-check)))))

(test-case "display-check-info-stack"
  (check-output "name:       \"foo\"\nactual:     1\nexpected:   2\n"
                (thunk (display-check-info-stack
                        (list (make-check-name "foo")
                              (make-check-actual 1)
                              (make-check-expected 2))))))
