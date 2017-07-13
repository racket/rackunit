#lang racket/base

(provide check-error
         check-fail
         check-fail*
         check-fail/info)

(require (for-syntax racket/base)
         racket/function
         rackunit/log
         syntax/parse/define
         "base.rkt"
         "check.rkt"
         "check-info.rkt")


(define-check (check-fail pred-or-msg chk-thnk)
  (contract-pred-or-msg! 'check-fail pred-or-msg)
  (contract-thunk! 'check-fail chk-thnk)
  (define failure (check-raise-value chk-thnk))
  (with-expected pred-or-msg
    (assert-failure failure)
    (assert-check-failure failure)
    (if (procedure? pred-or-msg)
        (unless (pred-or-msg failure)
          (with-actual failure
            (fail-check "Check failure didn't pass predicate")))
        (let ([msg (exn-message failure)])
          (unless (regexp-match? pred-or-msg msg)
            (with-actual msg
              (fail-check "Check failure message didn't match regexp")))))))

(define-check (check-fail/info expected-info chk-thnk)
  (contract-info! 'check-fail/info expected-info)
  (contract-thunk! 'check-fail/info chk-thnk)
  (define failure (check-raise-value chk-thnk))
  (with-expected expected-info
    (assert-failure failure)
    (assert-check-failure failure)
    (define infos (exn:test:check-stack failure))
    (define info-names (map check-info-name infos))
    (unless (for/or ([name (in-list info-names)])
              (equal? name (check-info-name expected-info)))
      (with-check-info (['actual-info-names info-names])
        (fail-check "Check failure did not contain the expected info")))
    (for ([info (in-list infos)])
      (when (equal? (check-info-name info) (check-info-name expected-info))
        (unless (equal? info expected-info)
          (with-actual info
            (fail-check
             "Check failure contained an unexpected info value")))))))

(define-check (check-fail* chk-thnk)
  (contract-thunk! 'check-fail* chk-thnk)
  (define failure (check-raise-value chk-thnk))
  (assert-failure failure)
  (assert-check-failure failure))

(define-check (check-error pred-or-msg chk-thnk)
  (contract-pred-or-msg! 'check-error pred-or-msg)
  (contract-thunk! 'check-error chk-thnk)
  (define failure (check-raise-value chk-thnk))
  (with-expected pred-or-msg
    (assert-failure failure)
    (assert-not-check-failure failure)
    (cond
      [(procedure? pred-or-msg)
       (unless (pred-or-msg failure)
         (with-actual failure
           (fail-check "Raised check error didn't pass predicate")))]
      [(exn? failure)
       (define msg (exn-message failure))
       (unless (regexp-match? pred-or-msg msg)
         (with-actual msg
           (fail-check "Raised check error message didn't match regexp")))]
      [else
       (with-actual failure
         (fail-check "Non-exception check error raised, no message present"))])))

;; Shorthands for adding infos

(define-simple-macro (with-actual act:expr body:expr ...)
  (with-check-info* (list (make-check-actual act)) (λ () body ...)))

(define-simple-macro (with-expected exp:expr body:expr ...)
  (with-check-info* (list (make-check-expected exp)) (λ () body ...)))

;; Pseudo-contract helpers, to be replaced with real check contracts eventually

(define (contract-pred-or-msg! name pred-or-msg)
  (unless (or (procedure? pred-or-msg) (regexp? pred-or-msg))
    (define ctrct "(or/c (-> any/c boolean?) regexp?)")
    (raise-argument-error name ctrct pred-or-msg)))

(define (contract-thunk! name thnk)
  (unless (procedure? thnk) (raise-argument-error name "(-> any)" thnk)))

(define (contract-info! name info)
  (unless (check-info? info) (raise-argument-error name "check-info?" info)))

;; Extracting raised values from checks

(define (check-raise-value chk-thnk)
  ;; To fully isolate the evaluation of a check inside another check,
  ;; we have to ensure that 1) the inner check raises its failure normally
  ;; instead of writing to stdout / stderr, 2) the inner check doesn't log
  ;; any pass or fail information to rackunit/log, and 3) the inner check's info
  ;; stack is independent of the outer check's info stack.
  (parameterize ([current-check-handler raise]
                 [test-log-enabled? #f]
                 [current-check-info (list)])
    (with-handlers ([(λ (e) (not (exn:break? e))) values]) (chk-thnk) #f)))

;; Assertion helpers

(define (assert-failure maybe-failure)
  (unless maybe-failure (fail-check "No check failure occurred")))

(define (assert-check-failure failure)
  (unless (exn:test:check? failure)
    (with-actual failure
      (fail-check "A value other than a check failure was raised"))))

(define (assert-not-check-failure failure)
  (when (exn:test:check? failure)
    (with-actual failure
      (fail-check "A non-error check failure was raised"))))
