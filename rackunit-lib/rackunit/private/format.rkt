#lang racket/base
(require racket/list
         racket/match
         racket/string
         "base.rkt"
         "check-info.rkt")

(provide display-test-result
         display-test-failure/error)

(module+ for-test
  (provide display-check-info-stack))

;; name-width : integer
;;
;; Number of characters we reserve for the check-info name column
(define minimum-name-width 12)

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

(define (string-pad-right s n)
  (define m (string-length s))
  (cond
   [(= m n) s]
   [(m . < . n)
    (string-append s (make-string (- n m) #\space))]
   [else
    (substring s 0 n)]))

(define (check-info-name-width check-info)
  (string-length
   (symbol->string
    (check-info-name check-info))))

(define (print-info-value v)
  (displayln (info-value->string v)))

(define (display-check-info-name-value max-name-width
                                       name
                                       value
                                       [value-printer print-info-value])
  (display (string-pad-right
            (string-append (symbol->string name) ": ")
            (max minimum-name-width (+ max-name-width 2))))
  (value-printer value))

(define (display-check-info max-name-width a-check-info)
  (match a-check-info
    [(struct check-info (name value))
     (display-check-info-name-value max-name-width name value)]))

(define (check-info-stack-max-name-width check-info-stack)
  (apply max 0
         (map check-info-name-width check-info-stack)))

;; display-check-info-stack : (listof check-info) -> void
(define (display-check-info-stack check-info-stack #:verbose? [verbose? #f])
  (define stack
    (if verbose? check-info-stack (simplify-params check-info-stack)))
  (define max-name-width (check-info-stack-max-name-width stack))
  (for ([info (in-list stack)])
    (display-check-info-name-value max-name-width
                                   (check-info-name info)
                                   (check-info-value info))))

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
          [else
           (display-raised-summary "ERROR" e)
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
