#lang racket/base
(require racket/contract/base
         racket/port
         "location.rkt"
         (for-syntax racket/base
                     racket/syntax))

(provide
 (contract-out
  [struct check-info ([name symbol?]
                      [value any/c])]
  [struct string-info ([value string?])]
  [info-value->string (-> any/c string?)]
  [check-info-mark symbol?]
  [check-info-stack (continuation-mark-set? . -> . (listof check-info?))]
  [current-check-info (-> (listof check-info?))]
  [with-check-info* ((listof check-info?) (-> any) . -> . any)])
 with-check-info)

;; Structures --------------------------------------------------

;; struct check-info : symbol any
(define-struct check-info (name value))

(struct string-info (value) #:transparent)

(define (info-value->string info-value)
  (if (string-info? info-value)
      (string-info-value info-value)
      (with-output-to-string (λ () (write info-value)))))

;; Infrastructure ----------------------------------------------

;; The continuation mark under which all check-info is keyed
(define check-info-mark (gensym 'rackunit))

;; (continuation-mark-set -> (listof check-info))
(define (check-info-stack marks)
  (let ([ht (make-hash)])
    (for ([x (in-list (apply append (continuation-mark-set->list marks check-info-mark)))]
          [i (in-naturals)])
      (hash-set! ht (check-info-name x) (cons i x)))
    (map cdr (sort (hash-map ht (λ (k v) v)) < #:key car))))

;; Shorthand to get the current check-info.
(define (current-check-info)
  (check-info-stack (current-continuation-marks)))

;; with-check-info* : (list-of check-info) thunk -> any
(define (with-check-info* info thunk)
  (define current-marks
    (continuation-mark-set-first #f check-info-mark))
  (with-continuation-mark
      check-info-mark
    (append (if current-marks current-marks null) info)
    (thunk)))

(define-syntax with-check-info
  (syntax-rules ()
    [(_ ((name val) ...) body ...)
     (with-check-info*
         (list (make-check-info name val) ...)
       (lambda () body ...))]))

(define-syntax (define-check-type stx)
  (syntax-case stx ()
    [(_ id contract)
     (with-syntax
         ([make-check-id (format-id #'id "make-check-~a" #'id)]
          [check-id? (format-id #'id "check-~a?" #'id)])
       (syntax/loc stx
         (begin (define (make-check-id a) (make-check-info 'id a))
                (define (check-id? info) (eq? (check-info-name info) 'id))
                (provide/contract
                 [make-check-id (contract . -> . check-info?)]
                 [check-id? (check-info? . -> . boolean?)]))))]))

(define-check-type name any/c)
(define-check-type params any/c)
(define-check-type location location/c)
(define-check-type expression any/c)
(define-check-type message any/c)
(define-check-type actual any/c)
(define-check-type expected any/c)
