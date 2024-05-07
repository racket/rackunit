#lang racket/base

(require racket/contract/base
         racket/list
         racket/port
         racket/pretty
         rackunit/log
         "location.rkt"
         (for-syntax racket/base
                     racket/syntax))

(provide
 (contract-out
  [struct check-info ([name symbol?]
                      [value any/c])]
  [struct string-info ([value string?])]
  [struct location-info ([value location/c])]
  [struct pretty-info ([value any/c])]
  [struct nested-info ([values (listof check-info?)])]
  [struct verbose-info ([value any/c])]
  [struct dynamic-info ([proc (-> any/c)])]
  [print-info-value (-> any/c any)]
  [info-value->string (-> any/c string?)]
  [current-check-info (parameter/c (listof check-info?))]
  [check-info-contains-key? (check-info-> symbol? boolean?)]
  [check-info-ref (check-info-> symbol? (or/c check-info? #f))]
  [with-check-info* ((listof check-info?) (-> any) . -> . any)]
  [with-default-check-info* ((listof check-info?) (-> any) . -> . any)])
 with-check-info)

(module+ for-test
  (provide trim-current-directory))

(define (check-info-> dom cod)
  (case-> (-> dom cod)
          (-> (listof check-info?) dom cod)))

;; Structures --------------------------------------------------

(struct check-info (name value)
  #:transparent #:constructor-name make-check-info)

(struct string-info (value) #:transparent)
(struct location-info (value) #:transparent)
(struct pretty-info (value) #:transparent)
(struct verbose-info (value) #:transparent)
(struct nested-info (values) #:transparent)
(struct dynamic-info (proc) #:transparent)

(define (info-value->string info-value)
  (with-output-to-string
    (lambda ()
      (print-info-value info-value))))

(define (print-info-value info-value)
  (cond
    [(string-info? info-value) (display (string-info-value info-value))]
    [(location-info? info-value)
     (display (trim-current-directory
                (location->string (location-info-value info-value))))]
    [(pretty-info? info-value)
     (pretty-print (pretty-info-value info-value) #:newline? #f)]
    [(verbose-info? info-value)
     (print-info-value (verbose-info-value info-value))]
    [else
     (write info-value)]))

(define (trim-current-directory path)
  (define cd (path->string (or (current-test-invocation-directory)
                               (current-directory))))
  (regexp-replace (regexp-quote cd) path ""))

;; Infrastructure ----------------------------------------------

(define current-check-info (make-parameter '()))

;; with-check-info* : (list-of check-info) thunk -> any
(define (with-check-info* info thunk)
  (define all-infos (append (current-check-info) info))
  (define infos/later-overriding-earlier
    (reverse (remove-duplicates (reverse all-infos) #:key check-info-name)))
  (force/info infos/later-overriding-earlier thunk))

;; with-default-check-info* : (listof check-info) thunk -> any
(define (with-default-check-info* info thunk)
  (define old-info (current-check-info))
  (define old-keys (map check-info-name old-info))
  (define (has-new-key? info)
    (not (memq (check-info-name info) old-keys)))
  (define new-info (filter has-new-key? info))
  (force/info (append old-info new-info) thunk))

;; force/info : (listof check-info) thunk -> any
(define (force/info info thunk)
  (parameterize ([current-check-info info])
    (thunk)))

(define-syntax with-check-info
  (syntax-rules ()
    [(_ ((name val) ...) body ...)
     (with-check-info*
         (list (make-check-info name val) ...)
       (lambda () body ...))]))

(define-syntax (define-check-type stx)
  (syntax-case stx ()
    [(_ id contract #:wrapper wrapper-proc)
     (with-syntax
         ([make-check-id (format-id #'id "make-check-~a" #'id)]
          [check-id? (format-id #'id "check-~a?" #'id)])
       (syntax/loc stx
         (begin
           (define (make-check-id a) (make-check-info 'id (wrapper-proc a)))
           (define (check-id? info) (eq? (check-info-name info) 'id))
           (provide/contract
            [make-check-id (contract . -> . check-info?)]
            [check-id? (check-info? . -> . boolean?)]))))]
    [(_ id contract)
     (syntax/loc stx (define-check-type id contract #:wrapper values))]))

(define-check-type name any/c)
(define-check-type params any/c #:wrapper pretty-info)
(define-check-type location location/c #:wrapper location-info)
(define-check-type expression any/c #:wrapper verbose-info)
(define-check-type message any/c)
(define-check-type actual any/c #:wrapper pretty-info)
(define-check-type expected any/c #:wrapper pretty-info)
(define-check-type tolerance any/c #:wrapper pretty-info)

(define check-info-ref
  (case-lambda
   [(k)
    (check-info-ref (current-check-info) k)]
   [(info k)
    (findf (λ (i) (eq? k (check-info-name i))) info)]))

(define check-info-contains-key?
  (case-lambda
   [(k)
    (check-info-contains-key? (current-check-info) k)]
   [(info k)
    (and (check-info-ref info k) #t)]))
