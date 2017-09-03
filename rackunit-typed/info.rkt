#lang info

(define collection "typed")

(define test-responsibles '((all jay)))

(define deps
  '("racket-index"
    "rackunit-gui"
    "rackunit-lib"
    "typed-racket-lib"
    "base"
    "testing-util-lib"))

(define pkg-desc "Typed Racket types for RackUnit")

(define pkg-authors '(samth stamourv))

(define version "1.0")
