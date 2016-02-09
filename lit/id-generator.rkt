#lang racket

(provide make-id-generator!) 

(define (make-id-generator! stx)
  (let ([h (make-hash)])
    (λ (s [stx stx] [srcloc #f] [props #f])
      (define sym (string->symbol (~a s)))
      (define num (hash-ref h sym 0))
      (hash-set! h sym (+ num 1))
      (datum->syntax stx
                     (string->symbol (~a sym num))
                     srcloc
                     props))))