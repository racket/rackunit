#lang racket/base

(require rackunit
         syntax/parse/define
         racket/format
         (for-syntax racket/base
                     syntax/location))

(define-syntax (current-file-name stx)
  (with-syntax ([file (syntax-source-file-name stx)])
    (syntax-case stx ()
      [_ #'file])))

(define-syntax (current-line-number stx)
  (with-syntax ([line (syntax-line stx)])
    (syntax-case stx ()
      [_ #'line])))

(define-simple-macro (with-cmd (args ...) e)
  (parameterize ([current-command-line-arguments (vector args ...)])
    (let () e)))

(define-simple-macro (run-test (args ...) e)
  (with-cmd (args ...)
    (let ([result (open-output-string)])
      (parameterize ([current-error-port result])
        (begin e (get-output-string result))))))

(define-simple-macro (check-error (args ...) e)
  (when (zero? (string-length (run-test (args ...) e)))
    (eprintf "TEST FAILED: (check-error ~s ~a)\n"
             (map ~a (list args ...)) (quote e))))

(define-simple-macro (check-no-error (args ...) e)
  (let ([result (run-test (args ...) e)])
    (unless (zero? (string-length result))
      (eprintf "TEST FAILED: (check-no-error ~s ~a)\n~a\n"
               (map ~a (list args ...)) (quote e) result))))

(define FILE-NAME (path->string (current-file-name)))

(module+ test
  ;; Define args for correct and incorrect file names.
  (define file-name-arg (format "file:~a" FILE-NAME))
  (define wrong-file-name-arg
    (format "file:~a"
            (list->string (map (compose integer->char add1 char->integer)
                               (string->list FILE-NAME)))))
  ;; Define test.
  (define (go)
    (with-check-info (['name 'foo])
      (check-equal? 1 2))) (define GO-LINE (current-line-number)) ;; Keep this on same line!
  ;; Define args for correct and incorrect line numbers.
  (define go-line-num-arg (format "line:~a" GO-LINE))
  (define wrong-go-line-num-arg (format "line:~a" (+ GO-LINE 100)))

  (check-error ("foo") (go))
  (check-no-error ("baz") (go))
  (check-error ("foo" "baz") (go))
  (check-error ("foo" file-name-arg) (go))
  (check-error ("foo" go-line-num-arg) (go))
  (check-no-error ("foo" wrong-file-name-arg) (go))
  (check-no-error ("foo" wrong-go-line-num-arg) (go))
  (check-no-error ("foo" file-name-arg wrong-go-line-num-arg) (go))
  (check-no-error ("foo" wrong-file-name-arg go-line-num-arg) (go))

  (define (go2)
    (check-equal? 2 3))

  (check-error () (go2))
  (check-no-error ("foo") (go2))
  )
