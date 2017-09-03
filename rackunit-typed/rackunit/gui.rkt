#lang typed/racket
(require typed/rackunit
         typed/private/utils)

(require/typed/provide
 rackunit/gui
 [test/gui
  (Test * -> Any)]
 [make-gui-runner
  (-> (Test * -> Any))])

;; this library transitively imports the gui framework and fails in Travis due
;; to gui-related configuration not being set, so we don't run it in tests
(module test racket/base)
