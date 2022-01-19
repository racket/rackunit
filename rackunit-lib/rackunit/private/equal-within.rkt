#lang racket/base

(provide equal?/within)

(require racket/flonum racket/extflonum)

;; equal?/within : Any Any Nonnegative-Real -> Boolean
(define (equal?/within a b delta)
  ;; equal-proc : Any Any -> Boolean
  (define (equal-proc a b)
    (cond
      [(and (number? a) (number? b))
       (<= (magnitude (- a b)) delta)]
      [(and (extflonum? a) (extflonum? b))
       (extfl<= (extflabs (extfl- a b)) (real->extfl delta))]
      [(and (flvector? a) (flvector? b))
       (and (= (flvector-length a) (flvector-length b))
            (for/and ([a (in-flvector a)] [b (in-flvector b)])
              (equal-proc a b)))]
      [(and (extflvector? a) (extflvector? b))
       (and (= (extflvector-length a) (extflvector-length b))
            (for/and ([a (in-extflvector a)] [b (in-extflvector b)])
              (equal-proc a b)))]
      [else
       (equal?/recur a b equal-proc)]))
  (equal-proc a b))

