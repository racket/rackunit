#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse)
         racket/contract/base
         racket/match
         rackunit/log
         syntax/parse/define
         "base.rkt"
         "equal-within.rkt"
         "check-info.rkt"
         "format.rkt"
         "location.rkt")

(provide
 (contract-out
  [fail-check (->* () (string?) void?)]
  [current-check-handler (parameter/c (-> any/c any))]
  [current-check-around (parameter/c (-> (-> any/c) any))]
  [plain-check-around (-> (-> void?) void?)]))

(provide check-around

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
         check-within
         check-not-false
         check-not-eq?
         check-not-eqv?
         check-not-equal?
         check-match
         fail)

(define current-check-handler (make-parameter display-test-failure/error))
(define (not-break-exn? x) (not (exn:break? x)))

;; Like default-check-around, except without test logging. This used to be used
;; by test-case, and is currently undocumented. Typed Racket's wrapper around
;; rackunit uses this (although it shouldn't) so we can't get rid of it yet.
;; Setting (current-check-handler) to `raise` makes this equivalent to
;; plain-check-around.
(define (check-around thunk)
  (define handler (current-check-handler))
  (with-handlers ([not-break-exn? handler]) (thunk)))

;; Evaluates a check just like a normal function, with no calls to test-log!
;; or the current check handler. Check failures are raised as plain exceptions.
(define (plain-check-around chk-thunk) (chk-thunk))

;; This is the default for current-check-around, and ensures a check logs
;; test results appropriately.
(define (default-check-around chk-thunk)
  (define handler (current-check-handler))
  (define (log-and-handle! e) (test-log! #f) (handler e))
  ;; Nested checks should be evaluated as normal functions, to avoid double
  ;; counting test results.
  (parameterize ([current-check-around plain-check-around])
    (with-handlers ([not-break-exn? log-and-handle!])
      (chk-thunk)
      (test-log! #t))))

(define current-check-around (make-parameter default-check-around))

(define (fail-check [message ""])
  (define marks (current-continuation-marks))
  (raise (make-exn:test:check message marks (current-check-info))))

;; refail-check : exn:test:check -> (exception raised)
;;
;; Raises an exn:test:check with the contents of the
;; given exception.  Useful for propogating internal
;; errors to the outside world.
(define (refail-check exn)
  (raise
   (make-exn:test:check (exn-message exn)
                        (exn-continuation-marks exn)
                        (exn:test:check-stack exn))))

(define (list/if . vs) (filter values vs))

(begin-for-syntax
  (require racket/syntax)
  ;; xform is the actual macro transformer procedure
  ;; impl-name is an identifier naming the `check-impl` procedure
  (struct check-transformer (xform impl)
    #:property prop:procedure 0)
  (provide check-transformer-impl-name check-transformer?)
  (define (check-transformer-impl-name s)
    (unless (check-transformer? s)
      (raise-argument-error
       'check-transformer-impl-name "check-transformer?" s))
    (check-transformer-impl s))
  (define-syntax-class check-name
    (pattern i:id
             #:with impl-name (format-id #f "~a-impl" #'i))))

(define-simple-macro (define-check-func (name:id formal:id ...) #:public-name pub:id body:expr ...)
  (define (name #:location [location (list 'unknown #f #f #f #f)]
                #:expression [expression 'unknown]
                #:check-around [check-around current-check-around])
    (procedure-rename
      (λ (formal ... [message #f])
        (when (and message (not (string? message)))
          (raise-argument-error 'pub "(or/c #f string?)" message))
        (define infos
          (list/if (make-check-name 'pub)
                   (make-check-location location)
                   (make-check-expression expression)
                   (make-check-params (list formal ...))
                   (and message (make-check-message message))))
        (with-default-check-info* infos
          (λ () ((check-around) (λ () body ... (void))))))
      'pub)))

(define-simple-macro (define-check (name:check-name formal:id ...) body:expr ...)
  (begin
    (define-check-func (name.impl-name formal ...) #:public-name name body ...)
    ;; (define check-impl (make-check-func (name.impl-name formal ...) #:public-name name body ...))
    (define-syntax name
      (check-transformer
       (lambda (stx)
         (with-syntax ([loc (datum->syntax #f 'loc stx)])
           (syntax-parse stx
             [(chk . args)
              #`(let ([location (syntax->location #'loc)])
                  (with-default-check-info*
                    (list (make-check-name 'name)
                          (make-check-location location)
                          (make-check-expression '(chk . args)))
                    #,(syntax/loc #'loc
                        (λ ()
                          ((current-check-around)
                           (λ ()
                             ((name.impl-name #:location location
                                              #:expression '(chk . args)
                                              #:check-around (λ () (λ (f) (f))))
                              . args)))))))]
             [chk:id
              #'(name.impl-name #:location (syntax->location #'loc)
                                #:expression 'chk)])))
       #'name.impl-name))))


(define-syntax-rule (define-simple-check (name param ...) body ...)
  (define-check (name param ...)
    (or (let () body ...) (fail-check))))

(define-syntax define-binary-check
  (syntax-rules ()
    [(_ (name expr1 expr2) body ...)
     (define-check (name expr1 expr2)
       (with-default-check-info*
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
              (with-default-check-info*
               (list
                (make-check-message "Wrong exception raised")
                (make-check-info 'exn-message (exn-message exn))
                (make-check-info 'exn exn))
               (lambda () (fail-check))))])
        (thunk))
      (with-default-check-info*
       (list (make-check-message "No exception raised"))
       (lambda () (fail-check))))))

(define-check (check-not-exn thunk)
  (raise-error-if-not-thunk 'check-not-exn thunk)
  (with-handlers
      ([exn:test:check? refail-check]
       [(and/c exn? not-break-exn?)
        (lambda (exn)
          (with-default-check-info*
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
  [(check-true expr) (eq? expr #t)]
  [(check-false expr) (eq? expr #f)]
  [(check-not-false expr) expr]
  [(check-not-eq? expr1 expr2) (not (eq? expr1 expr2))]
  [(check-not-eqv? expr1 expr2) (not (eqv? expr1 expr2))]
  [(check-not-equal? expr1 expr2) (not (equal? expr1 expr2))]
  [(fail) #f])

(define-check (check-= expr1 expr2 epsilon)
  (with-check-info*
   (list (make-check-actual expr1)
         (make-check-expected expr2)
         (make-check-tolerance epsilon))
   (lambda ()
     (unless (<= (magnitude (- expr1 expr2)) epsilon)
       (fail-check)))))

(define-check (check-within expr1 expr2 epsilon)
  (with-check-info*
   (list (make-check-actual expr1)
         (make-check-expected expr2)
         (make-check-tolerance epsilon))
   (lambda ()
     (unless (equal?/within expr1 expr2 epsilon)
       (fail-check)))))

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
       (with-default-check-info*
        (list (make-check-name 'check-match)
              (make-check-location
                (syntax->location (quote-syntax #,(datum->syntax #f 'loc stx))))
              (make-check-expression '#,(syntax->datum stx))
              (make-check-actual actual-val)
              (make-check-info 'pattern 'expected)
              #,@(cond [(eq? (syntax-e #'pred) #t) '()]
                       [else #'((make-check-info 'condition 'pred))]))
        (lambda ()
         (check-true (match actual-val
                       [expected pred]
                       [_ #f]))))))]
    [(_ actual expected)
     (syntax/loc stx (check-match actual expected #t))]))
