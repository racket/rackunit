#lang racket/base
(require racket/runtime-path
         rackunit
         racket/path)

(module test racket/base
  (require syntax/location)
  ;; Use a separate namespace to avoid logging results
  ;; in this namespace (where `raco test` would see errors).
  (parameterize ([current-namespace (make-base-namespace)])
    (dynamic-require (quote-module-path "..") #f)))

(define-runtime-path here ".")
(define collects 
  (normalize-path (build-path here ".." "..")))
(define (collect-trim bs)
  (regexp-replace* (regexp-quote (path->bytes collects)) bs #"PLTHOME/collects"))

(define (require&catch path)
  (define out-bs (open-output-bytes))
  (define err-bs (open-output-bytes))
  (parameterize ([current-output-port out-bs]
                 [current-error-port err-bs]
                 ;; Don't test error display handler output; it's too fragile.
                 [error-display-handler (λ (msg exn) (displayln msg))])
    (dynamic-require path #f))
  (close-output-port out-bs)
  (close-output-port err-bs)
  (values (collect-trim (get-output-bytes out-bs))
          (collect-trim (get-output-bytes err-bs))))

(define-syntax-rule (test-file pth out err)
  (begin
    (define-runtime-module-path mod (file pth))
    (define-values (cout cerr) (require&catch mod))
    (check-equal? cout out)
    (check-equal? cerr err)))

(test-file "standalone-check-test.rkt"
           #"Oh HAI!\nI didn't run\n"
           #"\
--------------------
ERROR
name:       check-pred
location:   standalone-check-test.rkt:44:0

Outta here!
--------------------
--------------------
FAILURE
name:       check
location:   standalone-check-test.rkt:48:0
params:     '(#<procedure:=> 1 2)
message:    0.0
--------------------
")

(test-file "standalone-test-case-test.rkt"
           #""
           #"\
--------------------
ERROR


First Outta here!
--------------------
--------------------
error
ERROR


Second Outta here!
--------------------
--------------------
FAILURE
name:       check-eq?
location:   standalone-test-case-test.rkt:23:12
actual:     1
expected:   2
--------------------
--------------------
failure
FAILURE
name:       check-eq?
location:   standalone-test-case-test.rkt:24:21
actual:     1
expected:   2
--------------------
")
