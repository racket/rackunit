#lang racket/base
(require racket/list
         racket/port
         racket/pretty
         racket/string
         "base.rkt"
         "check-info.rkt")

(provide display-test-result
         display-test-failure/error)

(module+ for-test
  (provide display-check-info-stack))

;; continuation-mark-set-parameter-value : Continuation-Mark-Set (Parameterof X) -> X
(module continuation-mark-set-parameter-value racket/base
  (require (only-in '#%paramz parameterization-key))
  (provide continuation-mark-set-parameter-value)
  (define (continuation-mark-set-parameter-value marks param)
    (call-with-parameterization
     (continuation-mark-set-first marks parameterization-key)
     param)))
(require 'continuation-mark-set-parameter-value)

;; name-width : integer
;;
;; Number of characters we reserve for the check-info name column
(define minimum-name-width 9)

(define nested-indent-amount 2)
(define nesting-level (make-parameter 0))
(define multi-line-indent-amount 2)

(define (display-test-result res
                             #:verbose? [verbose? #f]
                             #:suite-names [suite-names '()])
  (define cname (combine-names (test-result-test-case-name res) suite-names))
  (define (display-err e)
    (display-test-failure/error e cname #:verbose? verbose?))
  (cond [(test-failure? res) (display-err (test-failure-result res))]
        [(test-error? res) (display-err (test-error-result res))]
        [else (void)]))

(define (combine-names test-name suite-names)
  (define any-names? (or test-name (not (empty? suite-names))))
  (and any-names?
       (string-join (snoc (or test-name "Unnamed test") suite-names) " > ")))

(define (snoc v vs) (append vs (list v)))

(define (string-padding str desired-len)
  (make-string (max (- desired-len (string-length str)) 0) #\space))

(define (check-info-name-width check-info)
  (string-length
   (symbol->string
    (check-info-name check-info))))

(define (check-info-stack-name-width check-info-stack)
  (define widths (map check-info-name-width check-info-stack))
  (apply max 0 widths))

(define (print-check-info-stack stack* verbose?
                                #:name-width [name-width* minimum-name-width])
  (define stack (if verbose? stack* (simplify-params stack*)))
  (define name-width (max name-width* (check-info-stack-name-width stack)))
  (define (print-info info) (print-check-info info verbose? name-width))
  (for-each print-info stack))

(define (print-check-info info verbose? name-width)
  (define name (symbol->string (check-info-name info)))
  (define value (check-info-value info))
  (cond [(dynamic-info? value)
         (define new-info
           (make-check-info (check-info-name info)
                            ((dynamic-info-proc value))))
         (print-check-info new-info verbose? name-width)]
        [(nested-info? value)
         (print-name name)
         (newline)
         (parameterize ([nesting-level (add1 (nesting-level))])
           (print-check-info-stack (nested-info-values value) verbose? #:name-width name-width))]
        [else
         (define one-line-candidate
           (with-output-to-string
             (lambda ()
               (parameterize ([pretty-print-columns 'infinity])
                 (print-name name name-width)
                 (print-info-value value)))))
         (cond
           [(short-line? one-line-candidate)
            (print-name name name-width)
            (print-info-value value)
            (newline)]
           [else
            (print-name name)
            (newline)
            (display (make-string multi-line-indent-amount #\space))
            (print-info-value value)
            (newline)])]))

(define (print-name name [name-width #f])
  (define indent (make-string (* nested-indent-amount (nesting-level)) #\space))
  (define pad
    (cond
    [name-width (string-append "  " (string-padding name name-width))]
    [else ""]))
  (printf "~a~a:~a" indent name pad))

(define (short-line? line)
  (and (<= (string-length line) (pretty-print-columns))
       (not (string-contains? line "\n"))))

;; display-check-info-stack : (listof check-info) -> void
(define (display-check-info-stack stack #:verbose? [verbose? #f])
  (print-check-info-stack stack verbose?))

;; display-test-name : (U string #f) -> void
(define (display-test-name name)
  (displayln (or name "Unnamed test ")))

;; simplify-params : (list-of check-info) -> (list-of check-info)
;;
;; Remove any 'params infos if there are any 'actual infos, as the latter
;; usually duplicates values in the former. Also removes any verbose infos.
(define (simplify-params stack)
  (define has-actual? (ormap check-actual? stack))
  (define (reject? info)
    (or (verbose-info? (check-info-value info))
        (and has-actual? (check-params? info))))
   (filter-not reject? stack))

;; display-test-failure/error : any string/#f -> void
(define (display-test-failure/error e [name #f] #:verbose? [verbose? #f])
  (parameterize ((current-output-port (current-error-port)))
    (display-delimiter)
    (when name (display-test-name name))
    (cond [(exn:test:check? e)
           (display-raised-summary "FAILURE" e)
           (display-check-info-stack (exn:test:check-stack e)
                                     #:verbose? verbose?)
           (display-raised-message e)]
          [(exn? e)
           (display-raised-summary "ERROR" e)
           (display-check-info-stack (exn-check-info e)
                                     #:verbose? verbose?)
           (display-raised-message e)]
          [else
           (display-raised-summary "ERROR" e)
           (display-check-info-stack (current-check-info)
                                     #:verbose? verbose?)
           (display-raised-message e)])
    (display-delimiter)))

(define (display-raised-message v)
  (if (exn? v)
      (when (not (equal? (exn-message v) ""))
        (newline)
        (displayln (exn-message v)))
      (printf "\nA value other than an exception was raised: ~e\n" v)))

(define (display-delimiter) (displayln "--------------------"))

(define (display-raised-summary desc raised-value)
  (if (exn? raised-value)
      (parameterize ([error-print-context-length 0])
        ((error-display-handler) desc raised-value))
      (displayln desc)))

;; exn-check-info : Exn -> (Listof Check-Info)
(define (exn-check-info e)
  (continuation-mark-set-parameter-value (exn-continuation-marks e)
                                         current-check-info))
