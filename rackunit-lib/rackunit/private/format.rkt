#lang racket/base
(require racket/list
         racket/match
         racket/string
         racket/pretty
         "base.rkt"
         "check-info.rkt")

(provide display-test-result
         display-test-failure/error)

(module+ for-test
  (provide display-check-info-stack))

;; name-width : integer
;;
;; Number of characters we reserve for the check-info name column
(define minimum-name-width 9)

(define nested-indent-amount 2)
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

(define (string-indent str amnt)
  (define pad (make-string amnt #\space))
  (string-append pad (string-replace str "\n" (string-append "\n" pad))))

(define (check-info-name-width check-info)
  (string-length
   (symbol->string
    (check-info-name check-info))))

(define (check-info-stack-name-width check-info-stack)
  (define widths (map check-info-name-width check-info-stack))
  (apply max 0 widths))

(define (check-info-stack->string stack* verbose?
                                  #:name-width [name-width* minimum-name-width])
  (define stack (if verbose? stack* (simplify-params stack*)))
  (define name-width (max name-width* (check-info-stack-name-width stack)))
  (define (info->str info) (check-info->string info verbose? name-width))
  (string-join (map info->str stack) "\n"))

(define (check-info->string info verbose? name-width)
  (define name (symbol->string (check-info-name info)))
  (define value (check-info-value info))
  (cond [(dynamic-info? value)
         (define new-info
           (make-check-info (check-info-name info)
                            ((dynamic-info-proc value))))
         (check-info->string new-info verbose? name-width)]
        [(nested-info? value)
         (define nested-str (nested-info->string value verbose? name-width))
         (format "~a:\n~a" name nested-str)]
        [else
         (define pad (string-padding name name-width))
         (define one-line-candidate
           (parameterize ([pretty-print-columns 'infinity])
             (format "~a:~a  ~a" name pad (info-value->string value))))
         (if (<= (string-length one-line-candidate) (pretty-print-columns))
             one-line-candidate
             (format "~a:\n~a"
                     name
                     (string-indent
                      (info-value->string value)
                      multi-line-indent-amount)))]))

(define (nested-info->string nested verbose? name-width)
  (define infos (nested-info-values nested))
  (define nested-str
    (check-info-stack->string infos verbose? #:name-width name-width))
  (string-indent nested-str nested-indent-amount))

;; display-check-info-stack : (listof check-info) -> void
(define (display-check-info-stack stack #:verbose? [verbose? #f])
  (displayln (check-info-stack->string stack verbose?)))

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
