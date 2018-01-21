#lang racket/base

(require racket/contract/base
         racket/format
         racket/list
         racket/port
         racket/pretty
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
  [info-value->string (-> any/c string?)]
  [current-check-info (parameter/c (listof check-info?))]
  [check-info-contains-key? (check-info-> symbol? boolean?)]
  [check-info-ref (check-info-> symbol? (or/c check-info? #f))]
  [with-check-info* ((listof check-info?) (-> any) . -> . any)])
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
  (cond
    [(string-info? info-value) (string-info-value info-value)]
    [(location-info? info-value)
     (trim-current-directory
      (location->string (location-info-value info-value)))]
    [(pretty-info? info-value) (pretty-format (pretty-info-value info-value))]
    [(verbose-info? info-value)
     (info-value->string (verbose-info-value info-value))]
    [else (~s info-value)]))

(define (trim-current-directory path)
  (define cd (path->string (current-directory)))
  (regexp-replace (regexp-quote cd) path ""))

;; Infrastructure ----------------------------------------------

(define current-check-info (make-parameter '()))

;; with-check-info* : (list-of check-info) thunk -> any
(define (with-check-info* info thunk)
  (define all-infos (append (current-check-info) info))
  (define infos/later-overriding-earlier
    (reverse (remove-duplicates (reverse all-infos) #:key check-info-name)))
  (parameterize ([current-check-info infos/later-overriding-earlier])
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
