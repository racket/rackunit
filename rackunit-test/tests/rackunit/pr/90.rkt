#lang racket/base

;; Test the optional `message` argument to checks:
;; - if "", output contains ""
;; - if #f, output nothing
;; - if unsupplied, output nothing

(require rackunit
         rackunit/text-ui
         racket/port
         tests/eli-tester)

;; --- setup

;; Test check forms with no message, #f message, and "" message
(define-syntax-rule (test-check/empty-message (check-name arg* ...) ...)
  (begin
    (begin
      (test (not (regexp-match? #rx"message:" (capture-output (check-name arg* ...)))))
      (test (not (regexp-match? #rx"message:" (capture-output (check-name arg* ... #f)))))
      (test (regexp-match? #rx"message: *\"\"" (capture-output (check-name arg* ... "")))))
    ...))

;; Evaluate `expr`, return string with error output
(define-syntax-rule (capture-output expr)
  (with-output-to-string
    (lambda ()
      (parameterize ([current-error-port (current-output-port)])
        expr))))

;; --- actual tests

(test-check/empty-message
  (check-eq? 'a 'b)
  (check-not-eq? 0 0)
  (check-eqv? 'a 'b)
  (check-not-eqv? 0 0)
  (check-equal? 'a 'b)
  (check-not-equal? 0 0)
  (check-pred values #f)
  (check-= 0 1 0)
  (check-true #false)
  (check-false #true)
  (check-not-false #false)
  (check = 1 2)
  (fail)
)

;; --- copied from "../pr10950.rkt"

(module test racket/base
  (require syntax/location)
  ;; Use a separate namespace to avoid logging results
  ;; in this namespace (where `raco test` would see errors).
  (parameterize ([current-namespace (make-base-namespace)])
    (dynamic-require (quote-module-path "..") #f)))
