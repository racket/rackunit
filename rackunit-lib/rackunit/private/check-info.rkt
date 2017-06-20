#lang racket/base
(require racket/contract/base
         racket/format
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
  [info-value->string (-> any/c string?)]
  [current-check-info (parameter/c (listof check-info?))]
  [with-check-info* ((listof check-info?) (-> any) . -> . any)])
 with-check-info)

(module+ for-test
  (provide trim-current-directory))

;; Structures --------------------------------------------------

;; struct check-info : symbol any
(struct check-info (name value) #:constructor-name make-check-info)

(struct string-info (value) #:transparent)
(struct location-info (value) #:transparent)
(struct pretty-info (value) #:transparent)

(define (info-value->string info-value)
  (cond
    [(string-info? info-value) (string-info-value info-value)]
    [(location-info? info-value)
     (trim-current-directory
      (location->string (location-info-value info-value)))]
    [(pretty-info? info-value) (pretty-format (pretty-info-value info-value))]
    [else (~s info-value)]))

(define (trim-current-directory path)
  (define cd (path->string (current-directory)))
  (regexp-replace (regexp-quote cd) path ""))

;; Infrastructure ----------------------------------------------

(define current-check-info (make-parameter '()))

;; with-check-info* : (list-of check-info) thunk -> any
(define (with-check-info* info thunk)
  (parameterize ([current-check-info (append (current-check-info) info)])
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
(define-check-type expression any/c)
(define-check-type message any/c)
(define-check-type actual any/c #:wrapper pretty-info)
(define-check-type expected any/c #:wrapper pretty-info)
