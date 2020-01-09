#lang racket/base
(require (for-syntax racket/base)
         racket/contract/base
         rackunit/log
         syntax/parse/define
         "format.rkt"
         "base.rkt"
         "check.rkt")

(provide test-begin
         test-case
         before
         after
         around)

(provide
 (contract-out
  [current-test-name (parameter/c (or/c string? #f))]
  [current-test-case-around (parameter/c (-> (-> any) any))]))

(define current-test-name (make-parameter #f))

;; test-case-around : ( -> a) -> a
;;
;; Run a test-case immediately, printing information on failure
(define (default-test-case-around thunk)
  (with-handlers ([(λ (_) #t) log-and-handle!])
    (begin0
      (parameterize ((current-custodian (make-custodian)))
        (thunk))
      (test-log! #t))))

;; default-test-case-handler : any -> any
(define (default-test-case-handler e)
  (display-test-failure/error e (current-test-name)))

(define current-test-case-around (make-parameter default-test-case-around))

(define (log-and-handle! e)
  (test-log! #f)
  (when (exn:break? e) (raise e))
  (default-test-case-handler e))

(define (run-test-case test-thunk #:name [name (current-test-name)])
  (parameterize ([current-test-name name])
    ((current-test-case-around)
     (λ ()
       (parameterize ([current-check-around plain-check-around])
         (test-thunk))))))

(define-simple-macro (test-begin body:expr ...)
  ;; empty test-begin body is allowed
  (run-test-case (λ () (void) body ...)))

(define-simple-macro (test-case name body:expr ...)
  #:declare name (expr/c #'string?)
  ;; empty test-case body is allowed
  (run-test-case (λ () (void) body ...) #:name name.c))

(define-simple-macro (before setup:expr body:expr ...+)
  (dynamic-wind (λ () setup) (λ () body ...) void))

(define-simple-macro (after body:expr ...+ teardown:expr)
  (dynamic-wind void (λ () body ...) (λ () teardown)))

(define-simple-macro (around setup:expr body:expr ...+ teardown:expr)
  (dynamic-wind (λ () setup) (λ () body ...) (λ () teardown)))
