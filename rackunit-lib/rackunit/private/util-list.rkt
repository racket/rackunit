#lang racket/base

(provide list/if)

(define (list/if . vs) (filter values vs))
