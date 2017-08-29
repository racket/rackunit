;;;
;;; Time-stamp: <2009-06-11 17:11:22 noel>
;;;
;;; Copyright (C) 2005 by Noel Welsh.
;;;

;;; This library is free software; you can redistribute it
;;; and/or modify it under the terms of the GNU Lesser
;;; General Public License as published by the Free Software
;;; Foundation; either version 2.1 of the License, or (at
;;; your option) any later version.

;;; This library is distributed in the hope that it will be
;;; useful, but WITHOUT ANY WARRANTY; without even the
;;; implied warranty of MERCHANTABILITY or FITNESS FOR A
;;; PARTICULAR PURPOSE.  See the GNU Lesser General Public
;;; License for more details.

;;; You should have received a copy of the GNU Lesser
;;; General Public License along with this library; if not,
;;; write to the Free Software Foundation, Inc., 59 Temple
;;; Place, Suite 330, Boston, MA 02111-1307 USA

;;; Author: Noel Welsh <noelwelsh@yahoo.com>
;;
;;
;; Commentary:

#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [run-tests (->* ((or/c test-case? test-suite?))
                  ((or/c 'quiet 'normal 'verbose))
                  natural-number/c)]))

(require racket/list
         racket/match
         rackunit/log
         "private/format.rkt"
         "private/test.rkt")


;; As we fold over the test results, there are two pieces of state we must
;; manage: a counter of failures, errors, and passes, and a stack of names
;; indicating where we are in the tree of test suites and cases. The former is
;; used for printing a summary message after running all tests, while the latter
;; is used in each test to print what cases / suites a test is in.
(struct fold-state (counter names) #:transparent)

(define init-state (fold-state (hash 'success 0 'failure 0 'error 0) (list)))

(define (push-suite-name name state)
  (struct-copy fold-state state [names (cons name (fold-state-names state))]))

(define (pop-suite-name name state)
  (struct-copy fold-state state [names (rest (fold-state-names state))]))

(define (result-type res)
  (cond [(test-success? res) 'success]
        [(test-failure? res) 'failure]
        [(test-error? res) 'error]))

(define (increment-counter res state)
  (define type (result-type res))
  (define new-counter (hash-update (fold-state-counter state) type add1))
  (struct-copy fold-state state [counter new-counter]))

(define (num-unsuccessful cnt)
  (+ (hash-ref cnt 'failure) (hash-ref cnt 'error)))

(define (log-counter! cnt)
  (for ([_ (in-range (hash-ref cnt 'success))]) (test-log! #t))
  (for ([_ (in-range (num-unsuccessful cnt))]) (test-log! #f)))

(define (display-counter cnt)
  (match cnt
    [(hash-table ['success s] ['failure f] ['error e])
     (define total (+ s f e))
     (define tests-passed? (and (zero? f) (zero? e)))
     (define (print-msg)
       (printf "~a success(es) ~a failure(s) ~a error(s) ~a test(s) run\n"
               s f e total))
     (if tests-passed?
         (print-msg)
         (parameterize ([current-output-port (current-error-port)])
           (print-msg)))]))

(define (run-tests test [mode 'normal])
  (define quiet? (eq? mode 'quiet))
  (define (print-result result state)
    (unless quiet?
      (define verbose? (eq? mode 'verbose))
      (define names (fold-state-names state))
      (display-test-result result #:verbose? verbose? #:suite-names names))
    (increment-counter result state))
  (define (fold-tests)
    (fold-test-results print-result init-state test
                       #:fdown push-suite-name
                       #:fup pop-suite-name))
  ;; we install a new custodian to ensure resources handled improperly by tests
  ;; such as threads, tcp listeners, etc. don't interfere with other tests or
  ;; the runner itself
  (define final-counter (fold-state-counter (call/new-custodian fold-tests)))
  (log-counter! final-counter)
  (unless quiet?
    (display-counter final-counter))
  (num-unsuccessful final-counter))

(define (call/new-custodian thnk)
  (parameterize ([current-custodian (make-custodian)]) (thnk)))
