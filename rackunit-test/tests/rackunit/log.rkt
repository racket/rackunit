#lang racket/base
(require rackunit
         rackunit/log)

(module test racket/base
  (require syntax/location)
  ;; Use a separate namespace to avoid logging results
  ;; in this namespace (where `raco test` would see errors).
  (parameterize ([current-namespace (make-base-namespace)])
    (dynamic-require (quote-module-path "..") #f)))

(define-syntax-rule (&& label stdout-e stdout-p)
  (let ()
    (define stdout-ev stdout-e)
    (define stdout-av stdout-p)
    (unless (equal? stdout-ev stdout-av)
      (error 'log "bad ~a\n actual: ~v\n expected: ~v"
             label stdout-av stdout-ev))))

(define-syntax-rule (& test-e stdout-e stderr-e exit-e)
  (let ()
    (define stdout-p (open-output-string))
    (define stderr-p (open-output-string))
    (define exit-av 0)
    (parameterize ([current-output-port stdout-p]
                   [current-error-port stderr-p]
                   [exit-handler (λ (ec) (set! exit-av ec))])
      test-e)
    (&& 'stdout stdout-e (get-output-string stdout-p))
    (&& 'stderr stderr-e (get-output-string stderr-p))
    (&& 'exit-code exit-e exit-av)))

(& (test-log) "" "" 0)
(& (test-log #:display? #t) "" "" 0)
(& (test-log #:exit? #t) "" "" 0)
(& (test-log #:display? #t #:exit? #t) "" "" 0)

(check-true #t)

(& (test-log) "" "" 0)
(& (test-log #:display? #t) "1 test passed\n" "" 0)
(& (test-log #:exit? #t) "" "" 0)
(& (test-log #:display? #t #:exit? #t) "1 test passed\n" "" 0)

(parameterize ([current-error-port (current-output-port)])
  (check-true #f))

(& (test-log) "" "" 0)
(& (test-log #:display? #t) "" "1/2 test failures\n" 0)
(& (test-log #:exit? #t) "" "" 1)
(& (test-log #:display? #t #:exit? #t) "" "1/2 test failures\n" 1)

(parameterize ([test-log-enabled? #f])
  (check-true #t)
  (& (test-log) "" "" 0)
  (& (test-log #:display? #t) "" "1/2 test failures\n" 0)
  (& (test-log #:exit? #t) "" "" 1)
  (& (test-log #:display? #t #:exit? #t) "" "1/2 test failures\n" 1))
