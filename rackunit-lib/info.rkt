#lang info

(define collection 'multi)

(define deps '("base"
               ("testing-util-lib" #:version "1.1")))

(define implies '("testing-util-lib"))

(define pkg-desc "RackUnit testing framework")

(define pkg-authors '(ryanc noel))

(define version "1.8")
