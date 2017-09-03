#lang racket/base

(require rackunit
         "check-test.rkt"
         "format-test.rkt"
         "test-case-test.rkt"
         "test-suite-test.rkt"
         "base-test.rkt"
         "location-test.rkt"
         "result-test.rkt"
         "test-test.rkt"
         "util-test.rkt"
         "text-ui-test.rkt")

(provide all-rackunit-tests
         failure-tests)

(define all-rackunit-tests
  (test-suite
   "All RackUnit Tests"
   base-tests
   test-case-tests
   test-suite-tests
   test-suite-define-provide-test
   location-tests
   test-tests
   util-tests
   text-ui-tests
   format-tests
   ))

(define failure-tests
  (test-suite
   "Failures"
   (test-case "Intended to fail" (fail))
   (test-case "Also intended to fail" (check-eq? 'apples 'orange))
   (test-equal? "Yet again intended to fail" "apples" "oranges")
   (test-case "Intended to throw error" (error 'testing "<<This is an error message>>"))
   (test-case "Error within a check" (check error 'foo 'bar))
   ))
