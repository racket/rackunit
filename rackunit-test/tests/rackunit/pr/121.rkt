#lang racket/base
(require rackunit racket/port)
(module+ test
  (define s (make-semaphore))
  (define op (current-output-port) #;(open-output-nowhere))
  (define answer (make-channel))
  (define t
    (thread
     (λ ()
       (parameterize ([current-error-port op]
                      [current-output-port op])
         (with-handlers ([exn:break? (λ (x) (channel-put answer 'passed))])
           (check-equal? (let ()
                           (semaphore-post s)
                           (semaphore-wait (make-semaphore 0)))
                         5)
           (channel-put answer 'failed))))))
  (semaphore-wait s)
  (break-thread t)
  (unless (equal? (channel-get answer) 'passed)
    (error "test for pr 121 failed")))
