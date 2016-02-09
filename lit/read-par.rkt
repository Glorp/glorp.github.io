#lang racket
(provide (contract-out
          [read-par (-> (or/c path? #f) input-port? (listof text-bit?))]
          [text-bit? contract?]))

(require "read-util.rkt")

(define text-bit?
  (or/c
    (list/c 'norm string?)
    (list/c 'em string?)
    (list/c 'strong string?)
    (list/c 'tt string?)
    (list/c 'code syntax?)))

(define (read-par src in)
  (define res '())
  (define mode 'norm)
  (define text-port (open-output-string))
  (define quotation-mark (quotation-marker))
  
  (define (flush-text [m 'norm])
    (unless (equal? mode m)
      (error 'read-par "mode-mismatch; flushing ~s-text with ~s" mode m))
    (define str (get-output-string text-port))
    (when (non-empty-string? str)
      (set! res (cons `(,mode ,str) res)))
    (set! text-port (open-output-string)))

  (define (read-code)
    (set! res (cons `(code ,(read-syntax src in)) res)))

  (define (switch-mode m)
    (when (equal? m 'norm)
      (error 'switch-mode "plz don't switch with ~s" m))
    (cond [(equal? mode m) (flush-text m)
                           (set! mode 'norm)]
          [else (unless (equal? mode 'norm)
                  (error 'switch-mode "plz don't switch from ~s" mode))
                (flush-text)
                (set! mode m)]))
          
  
  (let loop ()
    (define c (read-char in))
    (cond [(eof-object? c)
           (void)]
          [(and (equal? c #\newline)
                (or (eof-object? (peek-char in))
                    (equal? (peek-char in) #\newline)
                    (equal? (peek-string 2 0 in) "#:")))
           (skip-newlines in)]
          [(equal? c #\")
           (write-char (quotation-mark) text-port)
           (loop)]
          [(equal? c #\')
           (write-char apo text-port)
           (loop)]
          [(equal? c #\\)
           (write-char (read-char in) text-port)
           (loop)]
          [(equal? c #\@)
           (flush-text)
           (read-code)
           (loop)]
          [(equal? c #\*)
           (switch-mode 'em)
           (loop)]
          [(equal? c #\_)
           (switch-mode 'strong)
           (loop)]
          [(equal? c #\¨)
           (switch-mode 'tt)
           (loop)]
          [else
           (write-char c text-port)
           (loop)]))

  (flush-text)
  (reverse res))



(module+ main
  (define (test-read s)
    (let ([i (open-input-string s)])
      (read-par #f i)))
  (test-read
"en også
to også
tre også
fire *fem* seks _syv!_ ¨hei¨

"))