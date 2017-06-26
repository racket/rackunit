#lang racket/base
(require racket/list
         racket/match
         "base.rkt"
         "check-info.rkt")

(provide display-check-info-name-value
         display-check-info
         display-check-info-stack
         display-test-name
         display-exn

         display-delimiter
         display-failure
         display-error

         display-test-failure/error
         strip-redundant-params
         check-info-stack-max-name-width

         display-verbose-check-info
         sort-stack)

;; name-width : integer
;;
;; Number of characters we reserve for the check-info name column
(define minimum-name-width 12)

(define (display-delimiter)
  (display "--------------------") (newline))

(define (display-failure)
  (display "FAILURE"))

(define (display-error)
  (display "ERROR"))

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

;; display-verbose-check-info : test-result -> void
(define (display-verbose-check-info result)
  (cond
    ((test-failure? result)
     (let* ((exn (test-failure-result result))
            (stack (exn:test:check-stack exn)))
       (display-verbose-check-info-stack stack)))
    ((test-error? result)
     (display-exn (test-error-result result)))
    (else
     (void))))

(define (display-verbose-check-info-stack check-info-stack)
  (define max-name-width (check-info-stack-max-name-width check-info-stack))
  (for ([info (in-list (sort-stack check-info-stack))])
    (cond
      ((check-expression? info)
       (display-check-info-name-value max-name-width
                                      (check-info-name info)
                                      (check-info-value info)
                                      (λ (x) (printf "~.s\n" x))))
      (else
       (display-check-info-name-value max-name-width
                                      (check-info-name info)
                                      (check-info-value info))))))

(define (check-info-stack-max-name-width check-info-stack)
  (apply max 0
         (map check-info-name-width check-info-stack)))

;; display-check-info-stack : (listof check-info) -> void
(define (display-check-info-stack check-info-stack)
  (display-verbose-check-info-stack
   (strip-redundant-params check-info-stack)))

;; display-test-name : (U string #f) -> void
(define (display-test-name name)
  (displayln (or name "Unnamed test ")))

;; display-exn : any -> void
;;
;; Outputs a printed representation of the exception to
;; the current-output-port
;; If given non-exn value, says so.
(define (display-exn v)
  (parameterize ((current-error-port (current-output-port)))
    (if (exn? v)
        ((error-display-handler) (exn-message v) v)
        (printf "A value other than an exception was raised: ~e\n" v))
    (newline)))

;; strip-redundant-parms : (list-of check-info) -> (list-of check-info)
;;
;; Strip any 'params infos if there are any 'actual infos, as the latter usually
;; duplicates values in the former.
(define (strip-redundant-params stack)
  (if (ormap check-actual? stack) (filter-not check-params? stack) stack))

;; display-test-failure/error : any string/#f -> void
(define (display-test-failure/error e [name #f])
  (parameterize ((current-output-port (current-error-port)))
    (display-delimiter)
    (when name (display-test-name name))
    (cond [(exn:test:check? e)
           (display-failure) (newline)
           (display-check-info-stack (exn:test:check-stack e))
           (unless (equal? (exn-message e) "")
             (newline)
             (parameterize ([error-print-context-length 0])
               ((error-display-handler) (exn-message e) e)))]
          [else
           (display-error) (newline)
           (display-exn e)])
    (display-delimiter)))

(define (sort-stack l)
  (sort l <
        #:key 
        (λ (info)
          (cond
            [(check-name? info)
             0]
            [(check-location? info)
             1]
            [(check-params? info)
             2]
            [(check-actual? info)
             3]
            [(check-expected? info)
             4]
            [else
             5]))))
