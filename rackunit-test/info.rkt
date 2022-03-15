#lang info

(define collection 'multi)

(define deps '())

(define pkg-desc "RackUnit documentation")

(define pkg-authors '(noel ryanc))
(define build-deps '("base"
                     "eli-tester"
                     "typed-racket-lib"
                     "rackunit-typed"
                     "rackunit-lib"))
(define update-implies '("rackunit-lib"))

(define license
  '(Apache-2.0 OR MIT))
