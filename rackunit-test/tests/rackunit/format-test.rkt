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
                              (make-check-expected 2)))))
  (test-case "string-info"
    (check-output "name:       foo\n"
                  (thunk (display-check-info-stack
                          (list (make-check-info 'name (string-info "foo")))))))
  (test-case "nested-info"
    (check-output "name:\n  foo:        1\n  bar:        2\n"
                  (thunk
                   (display-check-info-stack
                    (list (make-check-name
                           (nested-info
                            (list (make-check-info 'foo 1)
                                  (make-check-info 'bar 2)))))))))
  (test-case "dynamic-info"
    (check-output "foo:        1\n"
                  (thunk
                   (display-check-info-stack
                    (list (make-check-info 'foo (dynamic-info (thunk 1)))))))
    (test-case "with-nested-info"
      (check-output "name:\n  foo:        1\n  bar:        2\n"
                    (thunk
                     (display-check-info-stack
                      (list (make-check-name
                             (dynamic-info
                              (thunk
                               (nested-info
                                (list (make-check-info 'foo 1)
                                      (make-check-info 'bar 2)))))))))))))
