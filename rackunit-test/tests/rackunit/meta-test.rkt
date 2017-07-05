#lang racket/base

(module+ test
  (require rackunit)

  (define foo-info (make-check-info 'foo 'foo))
  (define-check (check-raise) (raise 'foo))

  ;; We test the meta checks using the meta checks themselves (essentially
  ;; testing them twice). Direct uses of the checks assume they work, while
  ;; individual test cases verify specific scenarios. This can be a bit
  ;; confusing, but given the self-referential nature of testing a testing
  ;; framework we've got to tie the knot somewhere.

  (test-case "meta checks fail on non-failing thunks"
    (check-fail #rx"No check failure occurred"
                (λ () (check-fail (λ (_) #t) void)))
    (check-fail #rx"No check failure occurred" (λ () (check-fail* void)))
    (check-fail #rx"No check failure occurred"
                (λ () (check-fail/info foo-info void)))
    (check-fail #rx"No check failure occurred"
                (λ () (check-error (λ (_) #t) void))))

  (test-case "non-error meta checks fail on non-failure raised values"
    (check-fail #rx"A value other than a check failure was raised"
                (λ () (check-fail (λ (_) #t) check-raise)))
    (check-fail #rx"A value other than a check failure was raised"
                (λ () (check-fail* check-raise)))
    (check-fail #rx"A value other than a check failure was raised"
                (λ () (check-fail/info foo-info check-raise))))

  (test-case "non-error meta checks add info for non-failure raised values"
    (define actual-info (make-check-actual 'foo))
    (check-fail/info actual-info (λ () (check-fail (λ (_) #t) check-raise)))
    (check-fail/info actual-info (λ () (check-fail* check-raise)))
    (check-fail/info actual-info (λ () (check-fail/info foo-info check-raise))))

  (test-case "check-fail adds an expected info"
    (check-fail/info (make-check-expected #rx"bar")
                     (λ () (check-fail #rx"bar" void))))

  (test-case "check-fail/info adds an expected info"
    (check-fail/info (make-check-expected foo-info)
                     (λ () (check-fail/info foo-info void))))

  (test-case "check-fail* passes for any check failure"
    (check-fail* fail))

  (test-case "check-fail asserts failure matches predicate or regexp"
    (define-check (fail/msg) (fail-check "Message!"))
    (check-fail #rx"sage" fail/msg)
    (check-fail* (λ () (check-fail #rx"notinmessage" fail/msg)))
    (check-fail (λ (e) (equal? (exn-message e) "Message!")) fail/msg)
    (check-fail*
     (λ ()
       (check-fail (λ (e) (equal? (exn-message e) "Not message!")) fail/msg))))

  (test-case "check-fail/info passes on failures with matching info"
    (define-check (fail/foo-info) (with-check-info* (list foo-info) fail-check))
    (check-fail/info foo-info fail/foo-info)
    (check-fail* (λ () (check-fail/info foo-info fail))))

  (test-case "check-fail/info adds info names on failure without expected info"
    (define info-names (list 'name 'location 'expression 'params))
    (check-fail/info (make-check-info 'actual-info-names info-names)
                     (λ () (check-fail/info foo-info fail))))

  (test-case "check-fail/info adds info value on failure with not-equal info"
    (define-check (fail/foo-bar) (with-check-info (['foo 'bar]) (fail-check)))
    (check-fail/info (make-check-actual (make-check-info 'foo 'bar))
                     (λ () (check-fail/info foo-info fail/foo-bar))))

  (test-case "check-error asserts check raises non-failure error matching predicate or regexp"
    (define-check (raise-foo) (raise 'foo))
    (check-error (λ (v) (equal? v 'foo)) raise-foo)
    (check-fail #rx"Raised check error didn't pass predicate"
                (λ () (check-error (λ (v) (equal? v 'bar)) raise-foo)))
    (define-check (raise-exn)
      (raise (make-exn "Message!" (current-continuation-marks))))
    (check-error #rx"sage" raise-exn)
    (check-fail #rx"Raised check error message didn't match regexp"
                (λ () (check-error #rx"notinmessage" raise-exn)))
    (check-fail #rx"Non-exception check error raised, no message present"
                (λ () (check-error #rx"foo" raise-foo)))
    (check-fail #rx"A non-error check failure was raised"
                (λ () (check-error (λ (_) #t) fail)))))
