#lang typed/racket
#:no-optimize ;; precaution, because of unsafe-provide
(require typed/racket/class
         typed/private/utils
         typed/private/rewriter
         "type-env-ext.rkt"
         (for-syntax syntax/parse syntax/srcloc racket/syntax)
         (for-syntax syntax/parse)
         (only-in rackunit check-transformer-impl-name)
         (only-in typed/racket/unsafe unsafe-provide))

(begin-for-syntax
  ;; this implements the behavior of `define-check`, but defers the
  ;; actual work to `impl-name`
  ;; impl-name : identifier?
  ;; stx : syntax?
  (define (call-impl stx impl-name)
    (with-syntax ([loc (build-source-location-list stx)])
      (syntax-case stx ()
        [(chk . args)
         #`((#,impl-name
             #:location 'loc
             #:expression '(chk . args))
            . args)]
        [chk:id
         #`(#,impl-name
            #:location 'loc
            #:expression 'chk)]))))

(define-syntax (define-tcheck stx)
  (syntax-case stx ()
    [(_ id)
     (let* ([impl-id (format-id #'id "~a-imp" #'id)])
     #`(begin
         (define-syntax (id stx)
           (call-impl stx #'#,impl-id))))]))

;; This module creates bindings for each of the impls of the
;; various checks we need, so that we have something to point
;; to with `require/typed`
(module impls racket/base
  (require rackunit (for-syntax racket/base racket/syntax))
  (define-syntax (get-impls stx)
    (syntax-case stx ()
      [(_ id ...)
       #`(begin
           #,@(for/list ([id (syntax->list #'(id ...))])
                (define id-impl (format-id #f "~a-imp" id))
                (define v (syntax-local-value id #f))
                (unless (check-transformer? v)
                  (raise-syntax-error #f "not a check-transformer" id))
                #`(begin (provide #,id-impl)
                         (define #,id-impl
                           #,(check-transformer-impl-name v)))))]))
  (get-impls check-equal? check-eq? check-eqv?
             check-not-equal? check-not-eq? check-not-eqv?
             check-true check-false check-not-false
             check-pred check-= check-exn check-not-exn
             check fail check-regexp-match))

;; this generates the `require/typed` of the impl, the `define-tcheck`
;; to create the macro wrapper that passes in the source location,
;; and the provide
(define-syntax (require-checks stx)
  (syntax-case stx ()
    [(require-checks [(id ...) t] ...)
     (with-syntax ([(([id-impl t] ...) ...)
                    (for/list ([ids (syntax->list #'((id ...) ...))]
                               [t (syntax->list #'(t ...))])
                      (for/list ([id (syntax->list ids)])
                        (list (format-id id "~a-imp" id) t)))])
     #'(begin
         (require/typed 'impls
                        [id-impl t] ... ...)
         (define-tcheck id) ... ...
         ;; use unsafe-provide in case untyped programs that import check-* from
         ;; typed/rackunit
         (unsafe-provide id ... ...)))]))
;; type for things like `check-equal?`
(define-type check-impl-ish-ty
  (-> #:location Any #:expression Any
      (case->
       (-> Any Any Any)
       (-> Any Any String Any))))

;; type for things like `check-true`
(define-type unary-check-impl-ish-ty
  (-> #:location Any #:expression Any
      (case->
       (-> Any Any)
       (-> Any String Any))))

;; all the actual specifications
(require-checks [(check-equal? check-eq? check-eqv?
                               check-not-equal? check-not-eq? check-not-eqv?)
                 check-impl-ish-ty]
                [(check-true check-false check-not-false)
                 unary-check-impl-ish-ty]
                [(check-pred)
                 (-> #:location Any #:expression Any
                     (All (A)
                          (case->
                           ((A -> Any) A -> Any)
                           ((A -> Any) A String -> Any))))]
                [(check-=)
                 (-> #:location Any #:expression Any
                     (case->
                      (Real Real Real -> Any)
                      (Real Real Real String -> Any)))]
                [(check-exn)
                 (-> #:location Any #:expression Any
                     (case->
                      ((U (Predicate Any) Regexp) (Thunk Any) -> Any)
                      ((U (Predicate Any) Regexp) (Thunk Any) String -> Any)))]
                [(check-not-exn)
                 (-> #:location Any #:expression Any
                     (case->
                      ((Thunk Any) -> Any)
                      ((Thunk Any) String -> Any)))]
                [(check)
                 (-> #:location Any #:expression Any
                     (All (A B)
                          (case->
                           ((A B -> Any) A B -> Any)
                           ((A B -> Any) A B String -> Any))))]
                [(fail)
                 (-> #:location Any #:expression Any
                     (->* () (String) Void))]
                [(check-regexp-match)
                 (-> #:location Any #:expression Any
                     (-> Regexp String Any Any))])

(define-type (Predicate A) (A -> Boolean))
(define-type (Thunk A) (-> A))

; 3.2

(require/typed rackunit/log
  [test-log! (Any -> Any)])

; 3.2.1
(require-typed-struct check-info
                      ([name : Symbol] [value : Any])
                      rackunit)
(define-type CheckInfo check-info)
(provide (struct-out check-info) CheckInfo)
(require/typed/provide
 rackunit
 [make-check-name (String -> CheckInfo)]
 [make-check-params ((Listof Any) -> CheckInfo)]
 [make-check-location ((List Any (Option Number) (Option Number) (Option Number) (Option Number)) -> CheckInfo)]
 [make-check-expression (Any -> CheckInfo)]
 [make-check-message (String -> CheckInfo)]
 [make-check-actual (Any -> CheckInfo)]
 [make-check-expected (Any -> CheckInfo)]
 [with-check-info* (All (A) ((Listof CheckInfo) (Thunk A) -> A))])
(require (only-in rackunit with-check-info))
(provide with-check-info)

; 3.2.2
(require (only-in rackunit define-simple-check define-binary-check define-check fail-check))
(provide define-simple-check define-binary-check define-check fail-check)

; 3.3
(require (prefix-in t: (except-in rackunit struct:check-info struct:exn:test struct:exn:test:check struct:test-result struct:test-failure
                                  struct:test-error struct:test-success)))
(define-syntax (test-begin stx)
  (syntax-case stx ()
    [(_ expr ...)
     (syntax/loc stx
       ((current-test-case-around)
        (lambda ()
          (with-handlers ([(ann
                             (λ (e)
                               (and (exn:fail? e)
                                    (not (exn:test? e))))
                             (-> Any Boolean : #:+ exn:fail))
                           (λ ([e : exn:fail])
                             (test-log! #f)
                             (raise e))])
          (parameterize ([current-check-handler raise])
            (void)
            expr ...)))))]
    [_
     (raise-syntax-error
      #f
      "Correct form is (test-begin expr ...)"
      stx)]))

(define-syntax (test-case stx)
  (syntax-case stx ()
    [(_ name expr ...)
     (quasisyntax/loc stx
       (parameterize
           ([current-test-name
             (ensure-string name (quote-syntax #,(datum->syntax #f 'loc #'name)))])
         (test-begin expr ...)))]))

(: ensure-string : Any Any -> String)
(define (ensure-string name src-stx)
  (unless (string? name)
    (raise-argument-error 'test-case "string?" name))
  name)

(unsafe-provide test-begin test-case)

(require/opaque-type TestCase test-case? rackunit)
(provide TestCase test-case?)

(define-type Seed Any)

(define-type test-suite-handler-down
  (rackunit-test-suite (Option String) (Thunk Any) (Thunk Any) Seed -> Seed))
(define-type test-suite-handler-up
  (rackunit-test-suite (Option String) (Thunk Any) (Thunk Any) Seed Seed -> Seed))
(define-type test-suite-handler-here
  (rackunit-test-case (Option String) (Thunk Any) Seed -> Seed))

(require/typed
 rackunit
 [#:struct rackunit-test-case ([name : (Option String)] [action : (Thunk Any)])
           #:constructor-name make-rackunit-test-case]
 [#:struct rackunit-test-suite
           ([name : String]
            [tests : (test-suite-handler-down
                      test-suite-handler-up
                      test-suite-handler-here
                      Seed -> Seed)]
            [before : (Thunk Any)]
            [after : (Thunk Any)])
           #:constructor-name make-rackunit-test-suite])
(require/typed
 rackunit/private/test-suite
 [apply-test-suite (rackunit-test-suite
                    test-suite-handler-down
                    test-suite-handler-up
                    test-suite-handler-here
                    Seed -> Seed)])

(define current-seed : (Parameter Seed)
  (make-parameter #f))

; taken directly from rackunit/private/test-suite
(: test-suite-test-case-around (test-suite-handler-here -> ((Thunk Any) -> Void)))
(define (test-suite-test-case-around fhere)
  (lambda (thunk)
    (let* ([name (current-test-name)]
           [test (make-rackunit-test-case name thunk)]
           [seed (current-seed)])
      (current-seed (fhere test name thunk seed)))))

; taken directly from rackunit/private/test-suite
(: test-suite-check-around (test-suite-handler-here -> ((Thunk Any) -> Void)))
(define (test-suite-check-around fhere)
  (lambda (thunk)
    (let* ([name #f]
           [test (make-rackunit-test-case name thunk)]
           [seed (current-seed)])
      (current-seed (fhere test name thunk seed)))))

; adapted from rackunit/private/test-suite
(define-syntax (test-suite stx)
  (syntax-parse stx
    [(_ name:expr
        (~or (~seq #:before before:expr) (~seq))
        (~or (~seq #:after after:expr) (~seq))
        test:expr ...)
     (with-syntax ([before (if (attribute before) #'before #'void)]
                   [after (if (attribute after) #'after #'void)])
       #'(let ([tests
                : (test-suite-handler-down
                   test-suite-handler-up
                   test-suite-handler-here
                   Seed -> Seed)
                (lambda (fdown fup fhere seed)
                  (define (run/inner [x : Any]) : Any
                    (cond [(rackunit-test-suite? x)
                           (current-seed
                            (apply-test-suite x fdown fup fhere (current-seed)))]
                          [(list? x)
                           (for-each run/inner x)]
                          [else
                           (void)]))
                  (parameterize
                      ([current-seed seed]
                       [current-test-case-around (test-suite-test-case-around fhere)]
                       [current-check-around (test-suite-check-around fhere)])
                    (let ([t : Any test])
                      (run/inner t))
                    ...
                    (current-seed)))])
           (make-rackunit-test-suite
            (ann name : String)
            tests
            (ann before : (Thunk Any))
            (ann after : (Thunk Any)))))]))
(unsafe-provide test-suite)

(define-type TestSuite rackunit-test-suite)
(provide TestSuite (rename-out [rackunit-test-suite? test-suite?]))

(define-type Test (U TestCase TestSuite))
(provide Test)

(require/typed/provide
 rackunit
 [make-test-suite
  (String (Listof (U TestCase TestSuite)) [#:before (Thunk Any)] [#:after (Thunk Any)] -> TestSuite)])

(require (only-in rackunit define-test-suite define/provide-test-suite))
(provide define-test-suite define/provide-test-suite)

(require/typed/provide
 rackunit
 [current-test-name (Parameter (Option String))]
 [current-test-case-around (Parameter ((Thunk Any) -> Any))])

; 3.3.1.1
(define-syntax-rule (def-test [tst (ch args ...)] ...)
  (begin (unsafe-provide tst ...)
         (define-syntax-rule (tst name args ...)
           (test-case name (ch args ...))) ...))

(def-test
  [test-check (check op v1 v2)]
  (test-pred (check-pred pred v))
  (test-equal? (check-equal? v1 v2))
  (test-eq? (check-eq? v1 v2))
  (test-eqv? (check-eqv? v1 v2))
  (test-= (check-= v1 v2 epsilon))
  (test-true (check-true v))
  (test-false (check-false v))
  (test-not-false (check-not-false v))
  (test-exn (check-exn pred thunk))
  (test-not-exn (check-not-exn thunk)))


; 3.4
(require (only-in rackunit before after around delay-test))
(provide before after around delay-test)

; 3.5
; XXX require/expose seems WRONG for typed/racket

; 3.7
(require-typed-struct (exn:test exn:fail) () rackunit)
(require-typed-struct (exn:test:check exn:test) ([stack : (Listof CheckInfo)]) rackunit)
(require-typed-struct test-result ([test-case-name : (Option String)]) rackunit)
(require-typed-struct (test-failure test-result) ([result : Any]) rackunit)
(require-typed-struct (test-error test-result) ([result : Any]) rackunit)
(require-typed-struct (test-success test-result) ([result : Any]) rackunit)
(provide (struct-out exn:test) (struct-out exn:test:check)
         (struct-out test-result)
         (struct-out test-failure) (struct-out test-error) (struct-out test-success))

(define-type (Tree A)
  (Rec The-Tree
       (Listof (U A The-Tree))))

(require/typed/provide
 rackunit
 [run-test-case
  ((Option String) (Thunk Any) -> test-result)]
 [run-test
  (Test -> (Tree test-result))]
 ; XXX Requires keywords and weird stuff
 #;[fold-test-results
    XXX]
 ; XXX Requires knowing more about test cases and structs
 #;[foldts-test-suite
    XXX])


; 5.1
(require/typed/provide
 rackunit
 [current-check-handler
  (Parameter (-> (U (Rec flat
                      (U Boolean Complex Char
                         Null Symbol String
                         Keyword (Pairof flat flat)))
                 exn)
                 Any))]
 [current-check-around
  (Parameter ((Thunk Any) -> Any))])
