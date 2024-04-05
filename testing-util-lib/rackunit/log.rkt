#lang racket/base

(provide (all-defined-out))
(require (prefix-in rt: raco/testing))

;; we don't immediately export the imported ids so that Scribble doesn't treat
;; them as re-exports.

(define test-log-enabled? rt:test-log-enabled?)
(define test-log! rt:test-log!)
(define test-log rt:test-report)
