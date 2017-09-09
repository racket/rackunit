#lang racket/base

(provide check-fail)

(require (for-syntax racket/base)
         racket/function
         racket/list
         rackunit/log
         syntax/parse/define
         rackunit
         (only-in rackunit/private/check-info
                  current-check-info
                  pretty-info)
         "private/util-list.rkt")


(define-check (check-fail tree chk-thnk)
  (contract-tree! 'check-fail tree)
  (contract-thunk! 'check-fail chk-thnk)
  (define failure (check-raise-value chk-thnk))
  (unless (exn:test:check? failure)
    (with-actual failure
      (fail-check "Check raised error instead of calling fail-check")))
  (check-tree-assert tree failure))

;; Shorthands for adding infos

(define-simple-macro (with-actual act:expr body:expr ...)
  (with-check-info* (error-info act) (λ () body ...)))

(define (error-info raised)
  (list/if (make-check-actual raised)
           (and (exn? raised)
                (make-check-info 'actual-message (exn-message raised)))
           (and (exn:test:check? raised)
                (make-check-info 'actual-info
                                 (nested-info
                                  (exn:test:check-stack raised))))))

;; Pseudo-contract helpers, to be replaced with real check contracts eventually

(define (contract-thunk! name thnk)
  (unless (and (procedure? thnk)
               (procedure-arity-includes? thnk 0))
    (raise-argument-error name "(-> any)" thnk)))

(define (contract-tree! name tree)
  (for ([v (in-list (flatten tree))])
    (unless (or (and (procedure? v)
                     (procedure-arity-includes? v 1))
                (regexp? v)
                (check-info? v))
      (define ctrct "(or/c (-> any/c any/c) regexp? check-info?)")
      (raise-argument-error name ctrct v))))

;; Extracting raised values from checks

(define (check-raise-value chk-thnk)
  ;; Checks called inside other checks raise their values normally and don't
  ;; log test failures, so all we have to do is ensure the check is executed
  ;; with an info stack that is independent of the outer check.
  (or (parameterize ([current-check-info (list)])
        (with-handlers ([(negate exn:break?) values]) (chk-thnk) #f))
      (fail-check "Check passed unexpectedly")))

;; Assertion helpers

(struct failure (type expected) #:transparent)

(define (assert-pred raised pred)
  (and (not (pred raised))
       (failure 'predicate pred)))

(define (assert-regexp exn rx)
  (and (not (regexp-match? rx (exn-message exn)))
       (failure 'message rx)))

(define (assert-info exn info)
  (and (not (member info (exn:test:check-stack exn)))
       (failure 'info info)))

(define (assert assertion raised)
  ((cond [(procedure? assertion) assert-pred]
         [(regexp? assertion) assert-regexp]
         [(check-info? assertion) assert-info])
   raised assertion))

(define (assertions-adjust assertions raised)
  (define is-exn? (exn? raised))
  (define has-regexps? (ormap regexp? assertions))
  (define adjust-regexps? (and has-regexps? (not is-exn?)))
  (if adjust-regexps?
      (cons exn? (filter-not regexp? assertions))
      assertions))

(define (assertion-tree-apply tree raised)
  (define assertions (assertions-adjust (flatten tree) raised))
  (filter-map (λ (a) (assert a raised)) assertions))

(define (failure-list->info failures)
  (define vs
    (if (equal? (length failures) 1)
        (pretty-info (failure-expected (first failures)))
        (nested-info (for/list ([f (in-list failures)])
                       (make-check-info (failure-type f)
                                        (pretty-info (failure-expected f)))))))
  (make-check-info 'expected vs))

(define (check-tree-assert tree raised)
  (with-actual raised
    (define failures (assertion-tree-apply tree raised))
    (unless (empty? failures)
      (with-check-info* (list (failure-list->info failures))
        fail-check))))
