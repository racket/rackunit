#lang racket/base

(module+ test
  (require rackunit
           rackunit/meta
           (only-in rackunit/private/check-info pretty-info))

  (define foo-info (make-check-info 'foo 'foo))
  (define-check (check-raise) (raise 'foo))

  ;; We test the meta checks using the meta checks themselves (essentially
  ;; testing them twice). Direct uses of the checks assume they work, while
  ;; individual test cases verify specific scenarios. This can be a bit
  ;; confusing, but given the self-referential nature of testing a testing
  ;; framework we've got to tie the knot somewhere.

  (define (accepts-no-args) (void))
  (define-check (fail/raise) (raise 'foo))
  (define-check (fail-foo) (fail-check "foo"))
  (define-check (fail-not-foo) (fail-check "bar"))
  (define some-info (make-check-info 'random 'info))
  (define-check (fail/info) (with-check-info* (list some-info) fail-check))
  (define (foo-fail? e) (equal? (exn-message e) "foo"))

  (test-case "check-fail"
    (check-fail '() fail-check)
    (check-fail foo-fail? fail-foo)
    (check-fail #rx"foo" fail-foo)
    (check-fail some-info fail/info)
    (check-fail #rx"Check passed unexpectedly" (λ () (check-fail '() void)))
    (check-fail #rx"Check raised error instead of calling fail-check"
                (λ () (check-fail '() fail/raise)))
    (check-fail (make-check-info 'expected (pretty-info foo-fail?))
                (λ () (check-fail foo-fail? fail-not-foo)))
    (check-fail (make-check-info 'expected (pretty-info #rx"foo"))
                (λ () (check-fail #rx"foo" fail-not-foo)))
    (check-fail (make-check-info 'expected (pretty-info some-info))
                (λ () (check-fail some-info fail-check)))
    (check-exn exn:fail:contract?
               (λ () (check-fail 'nonsense fail-check)))
    (check-exn exn:fail:contract?
               (λ () (check-fail (list #rx"foo" 'partial-nonsense)
                                 fail-check)))
    (check-exn exn:fail:contract?
               (λ () (check-fail accepts-no-args fail-check)))))
