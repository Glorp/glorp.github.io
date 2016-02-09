#lang racket
(provide read-meta)

(require "read-util.rkt")

(define (read-meta in)

  (define (read-key)
    (define str-port (open-output-string))

    (let loop ()
      (define c (read-char in))
      (cond [(equal? c #\:)
             (void)]
            [(or (equal? c #\newline) (eof-object? c))
             (error 'read-meta "~s while reading key" c)]
            [else
             (write-char c str-port)
             (loop)]))
    
    (string->symbol (get-output-string str-port)))

  (define (read-keyvals)    
    (define res (hash))
    (let loop ()
      (define p (peek-char in))
      (cond [(or (eof-object? p) (equal? p #\newline))
             (void)]
            [else
             (define k (read-key))
             (skip-spaces in)
             (define v (read-line in))
             (set! res (hash-set res k v))
             (loop)]))
    res)
  
  (skip-newlines in)
  (cond [(equal? (peek-string 6 0 in) "#:meta")
         (read-line in)
         (read-keyvals)]
        [else (hash)]))


(define (my-read str)
  (define in (open-input-string str))
  `(meta ,(read-meta in)))