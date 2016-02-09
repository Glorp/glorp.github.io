#lang racket

(provide read-end-line
         port-pos
         in-string
         skip-newlines
         skip-spaces
         quotation-marker
         apo)

(define (quotation-marker)
    (let ([l #\“]
          [r #\”]
          [next #t])
      (λ ()
        (define res (if next l r))
        (set! next (not next))
        res)))

(define apo #\’)

(struct port-position (line column pos) #:transparent)

(define (port-pos port)
  (define-values (l c p) (port-next-location port))
  (port-position l c p))

(define (read-end-line in)
    (skip-spaces in)
    (define c (read-char in))
    (unless (equal? c #\newline)
      (error 'read-end-line "expected newline, got ~s" c)))

(define (in-string str pos)
  (define port (open-input-string str))
  (port-count-lines! port)
  (match pos
    [(port-position l c p)
     (set-port-next-location! port l c p)
     port]))

(define (port-skipf in f)
  (define p (peek-char in))
  (cond [(eof-object? p) (read-char in)
                         (void)]
        [(f p) (read-char in)
               (port-skipf in f)]
        [else (void)]))

(define (skip-newlines in)
  (port-skipf in (λ (c) (equal? c #\newline))))

(define (skip-spaces in)
  (port-skipf in
              (λ (c) (and (not (equal? c #\newline))
                          (char-whitespace? c)))))