#lang racket/base

(module typed-success1 typed/racket/base
  (require typed/rackunit)
  (check-eq? 10 10))

(module typed-success2 typed/racket/base
  check-eq?
  (check-eq? 10 10)
  (require typed/rackunit))


(module typed-fail1 typed/racket/base
  (require typed/rackunit)
  (require (for-syntax syntax/parse syntax/srcloc racket/base))
  (require syntax/location)

  (define-syntax (def-test stx)
    (syntax-parse stx
      [(_ test-name loc-id expr)
       (define-values (_ fn __) (split-path (source-location-source stx)))
       (define loc-str (format "~a:~a:~a"
                               fn
                               (source-location-line #'expr)
                               (source-location-column #'expr)))
       #`(begin
           (define loc-id #,loc-str)
           (provide loc-id test-name)
           (define test-name (test-suite "test"
                                         (test-case "test1"
                                           expr))))]))
  (def-test
    test1
    test1-report-loc
    (check-eq? 10 20)))


(require rackunit racket/port rackunit/text-ui)
(require 'typed-fail1)

(define report
  (call-with-output-string
   (lambda (p)
     (parameterize ([current-error-port p]
                    [current-output-port p])
       (run-tests test1)))))

(module+ test
  (require (submod ".." typed-success1))
  (require (submod ".." typed-success2))
  (check-regexp-match (regexp-quote test1-report-loc)
                      report))
