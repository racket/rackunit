#lang racket/base

(module+ test

  (require racket/function
           racket/port
           rackunit
           (submod rackunit/private/format for-test))

  (define-check (check-info-stack-output str-or-rx stack)
    (define actual
      (with-output-to-string (thunk (display-check-info-stack stack))))
    (with-check-info (['actual actual] ['expected str-or-rx])
      (cond
        [(string? str-or-rx) (unless (equal? str-or-rx actual) (fail-check))]
        [(regexp? str-or-rx)
         (unless (regexp-match? str-or-rx actual) (fail-check))])))

  (test-case "Nested check info printing"
    (define test-info
      (list (make-check-info 'foo 1)
            (make-check-info 'nested
                             (nested-info (make-check-info 'bar 2)
                                          (make-check-info 'baz 3)))))
    (define expected-str "foo:        1
nested:
  bar:        2
  baz:        3
")
    (check-info-stack-output expected-str test-info))
  (test-case "Double-nested check info printing"
    (define test-info
      (list
       (make-check-info
        'nested (nested-info
                 (make-check-info
                  'double-nested (nested-info (make-check-info 'foo 1)
                                              (make-check-info 'bar 2)))))))
    (define expected-str "nested:
  double-nested:
    foo:            1
    bar:            2
")
    (check-info-stack-output expected-str test-info)))
