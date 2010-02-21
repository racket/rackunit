#lang scheme/base

(require (for-syntax scheme/base)
         "base.ss"
         "test-case.ss"
         "check.ss")

(provide test-suite
         test-suite-test-case-around
         test-suite-check-around
         delay-test
         make-test-suite

         apply-test-suite

         define-test-suite
         define/provide-test-suite)

(define (void-thunk) (void))

(define current-seed
  (make-parameter
   #f
   ;; Anything goes for the seed
   (lambda (v) v)))

(define (test-suite-test-case-around fhere)
  (lambda (thunk)
    (let* ([name (current-test-name)]
           [test (make-schemeunit-test-case name thunk)]
           [seed (current-seed)])
      (current-seed (fhere test name thunk seed)))))

(define (test-suite-check-around fhere)
  (lambda (thunk)
    (let* ([name #f]
           [test (make-schemeunit-test-case name thunk)]
           [seed (current-seed)])
      (current-seed (fhere test name thunk seed)))))


(define delayed-test-case-around
  (lambda (thunk)
    (let ([name (current-test-name)])
      (make-schemeunit-test-case name thunk))))

(define delayed-check-around
  (lambda (thunk)
    (let ([name #f])
      (make-schemeunit-test-case name thunk))))

(define-syntax delay-test
  (syntax-rules ()
    [(delay-test test test1 ...)
     (parameterize
                 ([current-test-case-around delayed-test-case-around]
                  [current-check-around delayed-check-around])
               test test1 ...)]))

(define (apply-test-suite suite fdown fup fhere seed)
  (let* ([name   (schemeunit-test-suite-name   suite)]
         [tests  (schemeunit-test-suite-tests  suite)]
         [before (schemeunit-test-suite-before suite)]
         [after  (schemeunit-test-suite-after  suite)]
         [kid-seed (fdown suite name before after seed)]
         [kid-seed ((schemeunit-test-suite-tests suite) fdown fup fhere kid-seed)])
    (fup suite name before after seed kid-seed)))

;; test-suite : name [#:before thunk] [#:after thunk] test ...
;;                     -> test-suite
;;
;; Creates a test-suite with the given name and tests.
;; Setup and teardown actions (thunks) may be specified by
;; preceding the actions with the keyword #:before or
;; #:after.
(define-syntax (test-suite stx)
  (syntax-case stx ()
    [(test-suite name
                 #:before before-thunk
                 #:after  after-thunk
                 test ...)
     (syntax
      (let ([the-name name]
            [the-tests
             (lambda (fdown fup fhere seed)
               (parameterize
                   ([current-seed seed]
                    [current-test-case-around (test-suite-test-case-around fhere)]
                    [current-check-around (test-suite-check-around fhere)])
                 (let ([t test])
                   (if (schemeunit-test-suite? t)
                       (current-seed (apply-test-suite t fdown fup fhere (current-seed)))
                       t))
                 ... (current-seed)))])
        (cond
         [(not (string? the-name))
          (raise-type-error 'test-suite "test-suite name as string" the-name)]
         [else
          (make-schemeunit-test-suite
           the-name
           the-tests
           before-thunk
           after-thunk)])))]
    [(test-suite name
                 #:before before-thunk
                 test ...)
     (syntax
      (test-suite name
                  #:before before-thunk
                  #:after  void-thunk
                  test ...))]
    [(test-suite name
                 #:after after-thunk
                 test ...)
     (syntax
      (test-suite name
                  #:before void-thunk
                  #:after  after-thunk
                  test ...))]
    [(test-suite name test ...)
     (syntax
      (test-suite name
                  #:before void-thunk
                  #:after  void-thunk
                  test ...))]))

(define (tests->test-suite-action tests)
  (lambda (fdown fup fhere seed)
    (parameterize
        ([current-seed seed])
      (for-each
       (lambda (t)
         (cond
          [(schemeunit-test-suite? t)
           (current-seed (apply-test-suite t fdown fup fhere (current-seed)))]
          [(schemeunit-test-case? t)
           (current-seed
            (fhere t
                   (schemeunit-test-case-name t)
                   (schemeunit-test-case-action t)
                   (current-seed)))]
          [else
           (raise
            (make-exn:test
             (format "tests->test-suite-action received ~a in list of tests ~a, which is not a test." t tests)
             (current-continuation-marks)))]))
       tests)
    (current-seed))))

;; make-test-suite : string [#:before thunk] [#:after thunk] (listof test?) -> test-suite?
;;
;; Construct a test suite from a list of tests
(define (make-test-suite name #:before [before void-thunk] #:after [after void-thunk] tests)
  (make-schemeunit-test-suite name
                              (tests->test-suite-action tests)
                              before
                              after))

;;
;; Shortcut helpers
;;

(define-syntax define-test-suite
  (syntax-rules ()
    [(define-test-suite name test ...)
     (define name
       (test-suite (symbol->string (quote name))
                   test ...))]))

(define-syntax define/provide-test-suite
  (syntax-rules ()
    [(define/provide-test-suite name test ...)
     (begin
       (define-test-suite name test ...)
       (provide name))]))