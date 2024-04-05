#lang racket/base

(require scribble/manual
         (for-syntax racket/base
                     syntax/parse
                     syntax/strip-context)
         (for-label raco/testing))

(provide raco-testing)

(define-syntax (raco-testing stx)
  (syntax-parse stx
    [(_ x)
     #:with x* (replace-context #'here #'x)
     #'(racket x*)]))
