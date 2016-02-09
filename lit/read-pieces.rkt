#lang racket
(provide (contract-out
          [read-pieces (-> syntax? procedure? (or/c path? #f) input-port? (listof piece?))]
          [piece? contract?]))

(require "read-par.rkt"
          "read-util.rkt"
          "id-generator.rkt")

(define piece?
  (or/c
   (list/c 'p hash? (listof text-bit?))
   (list/c 'heading hash? string?)
   (list/c 'top hash? string? (listof syntax?))
   (list/c 'eval hash? string? syntax?)
   (list/c 'hidden string? (listof syntax?))
   (list/c 'noex hash? string?)))

(define (~sym . args)
  (string->symbol (apply ~a
                         (map (λ (x) (if (syntax? x)
                                         (syntax->datum x)
                                         x))
                              args))))

(define (read-pieces stx get-id! src in)
  
  (define res '())
  (define prefix '||)
  (let loop ()
    (define piece (read-text-piece stx prefix get-id! src in))
    
    (match piece
      [`(heading ,h ,_) (set! prefix (~sym (hash-ref h '#:name) '-))]
      [_ (void)])
    
    (cond [(eof-object? piece) (void)]
          [else (set! res (cons piece res))
                (loop)]))
  (reverse res))

(define (read-code-str in)
  (define text-port (open-output-string))
  (define xtra-port (open-output-string))
  
  (let loop ()
    (cond [(or (equal? (peek-string 3 0 in) "\n#:")
               (eof-object? (peek-char in)))
           (read-char in)
           (void)]
          [(equal? (peek-char in) #\newline)
           (write-char (read-char in) xtra-port)
           (loop)]
          [else
           (define xtra (get-output-string xtra-port))
           (when (non-empty-string? xtra)
             (write-string xtra text-port)
             (set! xtra-port (open-output-string)))
           (write-char (read-char in) text-port)
           (loop)]))
  
  (get-output-string text-port))

(define (read-str/stx src in)
  (define start (port-pos in)) 
  
  (define str (read-code-str in))
  (define stx-port (in-string str start))
  
  (define stx-res '())
  (let loop ()
    (define stx (read-syntax src stx-port))
    (unless (eof-object? stx)
      (set! stx-res (cons stx stx-res))
      (loop)))
  
  (list str (reverse stx-res)))
           

(define (read-text-piece stx prefix get-id! src in)

  (define (read-keywords [start-hash (hash)] [read-name #t])
    (skip-spaces in)
    (define start-pos (port-pos in))
    (define str (read-line in))
    (define port (in-string str start-pos))
    (define res start-hash)
    
    (let loop ([read-name read-name])

      (define x (read-syntax src port))
      (define key (and (not (eof-object? x)) (syntax->datum x)))
      (cond [(eof-object? x)
             (void)]
            [(and (not (keyword? key)) read-name)
             (set! res (hash-set res '#:name x))
             (loop #f)]
            [(not (keyword? key))
             (error 'read-keywords "not a keyword: ~a" key)]
            [else
             (define value (read-syntax src port))
             (when (eof-object? value)
               (error 'read-keywords "missing value for key ~a" key))
             (set! res (hash-set res key value))
             (loop #f)]))

    (skip-newlines in)
    res)

  (define (get-heading-level keyword)
    (match keyword
      [#: 0]
      [#:# 1]
      [#:## 2]))

  (define-syntax-rule (default-name h v)
    (let ([x h])
      (if (hash-has-key? x '#:name)
          x
          (hash-set x '#:name (get-id! v)))))

  (skip-newlines in)
  (cond [(eof-object? (peek-char in))
         eof]
        
        [(equal? (peek-string 2 0 in) "#:")
         (match (read in)

           [#:p (define h (default-name (read-keywords (hash))
                              (~sym prefix 'par)))
                  `(p ,h ,(read-par src in))]
           
           [#:top (define h (default-name (read-keywords (hash))
                              (~sym prefix 'top)))                  
                  `(top ,h ,@(read-str/stx src in))]
           
           [#:eval (define h (default-name (read-keywords (hash))
                                           (~sym prefix 'eval)))
                   (match (read-str/stx src in)
                     [`(,str (,stx))
                      `(eval ,h ,str ,stx)])]

           [#:hidden (define h (default-name (read-keywords (hash))
                                 (~sym prefix 'hidden)))
                     `(hidden ,h ,@(read-str/stx src in))]
           
           [#:noex (define h (default-name (read-keywords (hash))
                                  (~sym prefix 'noex)))
                      `(noex ,h ,(read-code-str in))]

           [x (define level (get-heading-level x))
              (define name (read-syntax src in))
              (skip-spaces in)
              (define title (read-title in))
              (define h (read-keywords (hash '#:level level '#:name name) #f))
              `(heading ,h ,title)])]
          
        [else
         `(p ,(hash '#:name (get-id! (~sym prefix 'par))) ,(read-par src in))]))

(define (read-title in)
  (define quotation-mark (quotation-marker))
  (define text-port (open-output-string))
  (let loop ()
    (define c (read-char in))
    (cond [(or (eof-object? c) (equal? c #\newline)) (void)]
          [(equal? c #\") (write-char (quotation-mark) text-port)
                          (loop)]
          [(equal? c #\') (write-char apo text-port)
                          (loop)]
          [else (write-char c text-port)
                (loop)]))
  (get-output-string text-port))

(module+ main
  (define test-read (let ([get-id! (make-id-generator! #'e)]) (λ (s) (read-pieces #'e get-id! #f (open-input-string s)))))
  (test-read
"
et av
snitt ¨kode¨

#: meep En tittel
#:hesten: ro #:nilsen to #:kapekatt \"hei på deg\"
en også @(hest)
to også

tre også
fir

#:top tops #:en to
(define foo 1)
#:eval #:name foo
(foo x)
"))