#lang racket/base

(require (prefix-in ru: rackunit)
         (for-syntax
          racket/base syntax/parse
          typed-racket/utils/tc-utils
          typed-racket/env/init-envs
          typed-racket/rep/prop-rep
          typed-racket/rep/object-rep
          typed-racket/rep/type-rep
          typed-racket/types/abbrev))

(define-for-syntax unit-env
  (make-env
   [ru:current-test-case-around
    (-poly (a) (-> (-> a) a))]))

(begin-for-syntax (initialize-type-env unit-env))
