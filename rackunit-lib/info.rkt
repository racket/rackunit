#lang info

(define collection 'multi)

(define deps '("base"
               "testing-util-lib"))

(define implies '("testing-util-lib"))

(define pkg-desc "RackUnit testing framework")

(define pkg-authors '(ryanc noel))

(define version "1.11")

(define license
  '(Apache-2.0 OR MIT))
