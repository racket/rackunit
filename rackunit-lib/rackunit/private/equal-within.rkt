#lang racket/base

(provide equal?/within)

(require racket/flonum)

;; equal?/within : Any Any Nonnegative-Real -> Boolean
(define (equal?/within a b delta)
  ;; equal-proc : Any Any -> Boolean
  (define (equal-proc a b)
    (cond
      [(and (number? a) (number? b))
       (<= (magnitude (- a b)) delta)]
      [(and (flvector? a) (flvector? b))
       (and (= (flvector-length a) (flvector-length b))
            (for/and ([a (in-flvector a)] [b (in-flvector b)])
              (equal-proc a b)))]
      [else
       (equal?/recur a b equal-proc)]))
  (equal-proc a b))

