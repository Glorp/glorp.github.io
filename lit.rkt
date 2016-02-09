#lang racket

(provide (rename-out [lit-read read]
                     [lit-read-syntax read-syntax]))

(require "lit/read-util.rkt"
         "lit/id-generator.rkt"
         "lit/read-meta.rkt"
         "lit/read-pieces.rkt"
         "lit/assemble-program.rkt"
         (only-in racket/path file-name-from-path))

(define (lit-read in) (syntax->datum (lit-read-syntax #f in)))

(define (lit-read-syntax src in)

  (read-end-line in)
  (define name-string (if src
                          (path->string (path-replace-suffix (file-name-from-path src) ""))
                          "untitled"))
  (define name (read-syntax src (open-input-string name-string)))
  (define prefix (datum->syntax name
                                (string->symbol (~a (syntax->datum name) ':))))
  

  (define get-id! (make-id-generator! name))
  (define meta `(meta ,(hash-set (read-meta in)
                                 'file
                                 (path->string (syntax-source name)))))

  (define pieces (read-pieces name get-id! src in))
  (match-define `(,names ,res-stx ,eval-res-stx) (assemble-program name get-id! meta pieces))
  (define res (datum->syntax name
                             `(module ,name racket
                                (provide ,name
                                         ,@(map (λ (x) `(prefix-out ,prefix ,x)) names))
                                ,@res-stx
                                (module+ main
                                  ,@eval-res-stx))
                             name
                             name))
  ;(printf "~a~n" res)
  res)
  
