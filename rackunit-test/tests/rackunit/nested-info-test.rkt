#lang racket/base

(module+ test

  (require racket/function
           racket/port
           rackunit
           (submod rackunit/private/format for-test))

  (define (info* . name+vs)
    (define vec (list->vector name+vs))
    (define len (vector-length vec))
    (for/list ([name-idx (in-range 0 len 2)]
               [v-idx (in-range 1 len 2)])
      (make-check-info (vector-ref vec name-idx) (vector-ref vec v-idx))))

  (define (nested-info* . name+vs) (nested-info (apply info* name+vs)))
  
  (define-check (check-info-stack-output str-or-rx stack)
    (define actual
      (with-output-to-string (thunk (display-check-info-stack stack))))
    (with-check-info (['actual actual] ['expected str-or-rx])
      (unless (equal? str-or-rx actual) (fail-check))))

  (test-case "Nested check info printing"
    (define test-info (info* 'foo 1 'nested (nested-info* 'bar 2 'baz 3)))
    (define expected-str "foo:        1
nested:
  bar:        2
  baz:        3
")
    (check-info-stack-output expected-str test-info))
  (test-case "Double-nested check info printing"
    (define test-info
      (info* 'nested
             (nested-info* 'double-nested (nested-info* 'foo 1 'bar 2))))
    (define expected-str "nested:
  double-nested:
    foo:            1
    bar:            2
")
    (check-info-stack-output expected-str test-info)))
