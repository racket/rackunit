#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse)
         racket/contract/base
         racket/match
         rackunit/log
         syntax/parse/define
         "base.rkt"
         "check-info.rkt"
         "format.rkt"
         "location.rkt")

(provide
 (contract-out
  [fail-check (->* () (string?) void?)]))

(provide current-check-handler
         check-around
         current-check-around

         define-check
         define-binary-check
         define-simple-check

         check
         check-exn
         check-not-exn
         check-true
         check-false
         check-pred
         check-eq?
         check-eqv?
         check-equal?
         check-=
         check-not-false
         check-not-eq?
         check-not-eqv?
         check-not-equal?
         check-match
         fail)

;; parameter current-check-handler : (-> any any)
(define current-check-handler
  (make-parameter
   display-test-failure/error
   (lambda (v)
     (if (procedure? v)
         v
         (raise-type-error 'current-check-handler "procedure" v)))))

;; check-around : ( -> a) -> a
(define (check-around thunk)
  (with-handlers ([(lambda (e) #t) (current-check-handler)])
    (thunk)))

;; parameter current-check-around : (( -> a) -> a)
(define current-check-around
  (make-parameter
   (λ (thunk) (check-around thunk) (void))
   (lambda (v)
     (if (procedure? v)
         v
         (raise-type-error 'current-check-around "procedure" v)))))

(define (fail-check [message ""])
  (define marks (current-continuation-marks))
  (test-log! #f)
  (raise (make-exn:test:check message marks (current-check-info))))

;; refail-check : exn:test:check -> (exception raised)
;;
;; Raises an exn:test:check with the contents of the
;; given exception.  Useful for propogating internal
;; errors to the outside world.
(define (refail-check exn)
  (test-log! #f)
  (raise
   (make-exn:test:check (exn-message exn)
                        (exn-continuation-marks exn)
                        (exn:test:check-stack exn))))

(define (list/if . vs) (filter values vs))

(define-simple-macro
  (define-check-func (name:id formal:id ...) #:public-name pub:id body:expr ...)
  (define (name formal ... [message #f]
                #:location [location (list 'unknown #f #f #f #f)]
                #:expression [expression 'unknown])
    ((current-check-around)
     (lambda ()
       (with-check-info*
           (list/if (make-check-name 'pub)
                    (make-check-location location)
                    (make-check-expression expression)
                    (make-check-params (list formal ...))
                    (and message (make-check-message message)))
         (lambda () (begin0 (let () body ...) (test-log! #t))))))
    ;; All checks should return (void)
    (void)))

(define-simple-macro (define-check (name:id formal:id ...) body:expr ...)
  (begin
    (define-check-func (check-impl formal ...) #:public-name name body ...)
    (define-syntax (name stx)
      (with-syntax ([loc (datum->syntax #f 'loc stx)])
        (syntax-parse stx
          [(chk . args)
           #'(check-impl #:location (syntax->location #'loc)
                         #:expression '(chk . args)
                         . args)]
          [chk:id
           #'(lambda args
               (apply check-impl
                      #:location (syntax->location #'loc)
                      #:expression 'chk
                      args))])))))

(define-syntax-rule (define-simple-check (name param ...) body ...)
  (define-check (name param ...)
    (or (let () body ...) (fail-check))))

(define-syntax define-binary-check
  (syntax-rules ()
    [(_ (name expr1 expr2) body ...)
     (define-check (name expr1 expr2)
       (with-check-info*
        (list (make-check-actual expr1)
              (make-check-expected expr2))
        (lambda () (or (let () body ...) (fail-check)))))]
    [(_ (name pred expr1 expr2))
     (define-binary-check (name expr1 expr2) (pred expr1 expr2))]))

(define (raise-error-if-not-thunk name thunk)
  (unless (and (procedure? thunk)
               (procedure-arity-includes? thunk 0))
    (raise-arguments-error name "thunk must be a procedure that accepts 0 arguments" "thunk" thunk)))

(define-check (check-exn raw-pred thunk)
  (let ([pred
         (cond [(regexp? raw-pred)
                (λ (x) (and (exn:fail? x) (regexp-match raw-pred (exn-message x))))]
               [(and (procedure? raw-pred) (procedure-arity-includes? raw-pred 1))
                raw-pred]
               [else
                (raise-argument-error 'check-exn "(or/c (-> any/c any/c) regexp?)" raw-pred)])])
    (raise-error-if-not-thunk 'check-exn thunk)
    (let/ec succeed
      (with-handlers
          (;; catch the exception we are looking for and
           ;; succeed
           [pred
            (lambda (exn) (succeed #t))]
           ;; rethrow check failures if we aren't looking
           ;; for them
           [exn:test:check?
            (lambda (exn)
              (refail-check exn))]
           ;; catch any other exception and raise an check
           ;; failure
           [exn:fail?
            (lambda (exn)
              (with-check-info*
               (list
                (make-check-message "Wrong exception raised")
                (make-check-info 'exn-message (exn-message exn))
                (make-check-info 'exn exn))
               (lambda () (fail-check))))])
        (thunk))
      (with-check-info*
       (list (make-check-message "No exception raised"))
       (lambda () (fail-check))))))

(define-check (check-not-exn thunk)
  (raise-error-if-not-thunk 'check-not-exn thunk)
  (with-handlers
      ([exn:test:check? refail-check]
       [exn?
        (lambda (exn)
          (with-check-info*
           (list
            (make-check-message "Exception raised")
            (make-check-info 'exception-message (exn-message exn))
            (make-check-info 'exception exn))
           (lambda () (fail-check))))])
    (thunk)))

(define-syntax-rule (define-simple-check-values [header body ...] ...)
  (begin (define-simple-check header body ...) ...))

(define-simple-check-values
  [(check operator expr1 expr2) (operator expr1 expr2)]
  [(check-pred predicate expr) (predicate expr)]
  [(check-= expr1 expr2 epsilon)
   (<= (magnitude (- expr1 expr2)) epsilon)]
  [(check-true expr) (eq? expr #t)]
  [(check-false expr) (eq? expr #f)]
  [(check-not-false expr) expr]
  [(check-not-eq? expr1 expr2) (not (eq? expr1 expr2))]
  [(check-not-eqv? expr1 expr2) (not (eqv? expr1 expr2))]
  [(check-not-equal? expr1 expr2) (not (equal? expr1 expr2))]
  [(fail) #f])

(define-binary-check (check-eq? eq? expr1 expr2))
(define-binary-check (check-eqv? eqv? expr1 expr2))
(define-binary-check (check-equal? equal? expr1 expr2))

;; NOTE(jpolitz): This match form isn't eager like the others, hence the
;; define-syntax and the need to carry around location information
(define-syntax (check-match stx)
  (syntax-case stx ()
    [(_ actual expected pred)
     (quasisyntax
      (let ([actual-val actual])
       (with-check-info*
        (list (make-check-name 'check-match)
              (make-check-location
               (syntax->location (quote-syntax #,(datum->syntax #f 'loc stx))))
              (make-check-expression '#,(syntax->datum stx))
              (make-check-actual actual-val)
              (make-check-expected 'expected))
        (lambda ()
         (check-true (match actual-val
                       [expected pred]
                       [_ #f]))))))]
    [(_ actual expected)
     (syntax/loc stx (check-match actual expected #t))]))
