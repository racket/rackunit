#lang racket/base
(require racket/contract)

(define test-log-enabled? (make-parameter #t))

(define TOTAL 0)
(define FAILED 0)

(define-syntax-rule (inc! id)
  (set! id (add1 id)))

(define (test-log! result)
  (when (test-log-enabled?)
    (inc! TOTAL)
    (unless result
      (inc! FAILED))))

(define (test-log #:display? [display? #f]
                  #:exit? [exit? #f])
  (when display?
    (unless (zero? TOTAL)
      (cond
        [(zero? FAILED)
         (printf "~a test~a passed\n"
                 TOTAL
                 (if (= TOTAL 1) "" "s"))]
        [else
         (eprintf "~a/~a test failures\n"
                  FAILED TOTAL)])))
  (when exit?
    (unless (zero? FAILED)
      (exit 1)))
  (cons FAILED TOTAL))

(provide
 (contract-out
  [test-log-enabled? (parameter/c boolean?)]
  [test-log! (-> any/c void?)]
  [test-log (->* () (#:display? boolean? #:exit? boolean?)
                 (cons/c exact-nonnegative-integer?
                         exact-nonnegative-integer?))]))
