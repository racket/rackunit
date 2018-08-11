#lang racket/base
(require rackunit)

(module+ test
  (define ((arity-exn/rx rx) e)
    (and (exn:fail:contract:arity? e)
         (regexp-match? rx (exn-message e))))

  (check-exn (arity-exn/rx #rx"check-exn")
             (lambda () (check-exn (lambda () 'hi))))
  (check-exn (arity-exn/rx #rx"check-true")
             (lambda () (check-true 1 2 3 4)))
  (check-exn (arity-exn/rx #rx"check-equal\\?")
             (lambda () (check-equal? (list 1 2))))

  (let ([cp check-pred])
    (check-true (procedure-arity-includes? cp 2))
    (check-true (procedure-arity-includes? cp 3))
    (check-false (procedure-arity-includes? cp 0))
    (check-false (procedure-arity-includes? cp 1))
    (check-false (procedure-arity-includes? cp 4))))
