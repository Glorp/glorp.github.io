#lang racket
(provide (contract-out
          [assemble-program (-> syntax? procedure? meta? (listof piece?) any/c)]
          [meta? contract?]
          [program-syntax-list? contract?]))

(require (only-in "read-pieces.rkt" piece?))

(define program-syntax-list?
  (list/c (listof syntax?)
          (listof syntax?)
          (listof syntax?)))
                                
(define meta?
  (list/c 'meta hash?))

(define (assemble-program result-name get-id! meta pieces)

  (define stx result-name)
  (define res-stx '())
  (define format-res #'format-res)
  (define eval-res-stx (list (datum->syntax stx
                                            '(require racket/exn)
                                            stx
                                            stx)
                             (datum->syntax stx
                                            `(define (,format-res x)
                                               (match x
                                                 [(list 'error e)
                                                  (display (exn->string e) (current-error-port))]
                                                 [(list 'result x)
                                                  x]))
                                            stx
                                            stx)))
  (define names '())
  (define (add-stx stx)
    (set! res-stx (cons stx res-stx)))

  (define lit-res #'lit-res)
  (define get-pieces #'get-pieces)
  (define pieces-stx (datum->syntax stx 'pieces stx stx))
  (add-stx (datum->syntax stx
                          `(begin
                             (define ,lit-res '())
                             (define (add-piece p)
                               (set! ,lit-res (cons p ,lit-res)))
                             (define (,get-pieces)
                               (reverse ,lit-res)))
                          stx
                          stx))

  (match meta
    [`(meta ,h)
     (define name (datum->syntax stx 'meta stx stx))
     (add-stx (datum->syntax stx
                             `(define ,name
                                (make-immutable-hash ,(list 'quote (hash->list h))))))     
     (set! names (cons name names))])

  (for ([x pieces])
    (match x
      [`(heading ,h ,title)
       (define name (hash-ref h '#:name))
       (add-stx (datum->syntax stx
                               `(define ,name
                                  (list 'heading
                                        (make-immutable-hash ,(list 'quote (hash->list h)))
                                        ,title))
                               stx
                               stx))
       (add-stx (datum->syntax stx
                               `(add-piece ,name)))
       (set! names (cons name names))]
      
      [`(p ,h ,text)
       (define name (hash-ref h '#:name))
       (add-stx (datum->syntax stx
                             `(define ,name
                                (list 'p
                                      (make-immutable-hash ,(list 'quote (hash->list h)))
                                      (list ,@(map (λ (x)
                                                     (match x
                                                       [`(code ,stx) stx]
                                                       [x (list 'quote x)]))
                                                   text))))
                             stx
                             stx))
       (add-stx (datum->syntax stx
                               `(add-piece ,name)))
       (set! names (cons name names))]
      
      [`(top ,h ,str ,stxs)
       (define name (hash-ref h '#:name))
       (for ([stx stxs]) (add-stx stx))
       (add-stx (datum->syntax stx
                               `(define ,name (list 'top
                                                    (make-immutable-hash ,(list 'quote (hash->list h)))
                                                    ,str))))
       (add-stx (datum->syntax stx
                               `(add-piece ,name)))
       (set! names (cons name names))]

      [`(eval ,h ,str ,stx)
       (define name (hash-ref h '#:name))
       (add-stx (datum->syntax stx
                               `(define ,name (list 'eval
                                                    (make-immutable-hash ,(list 'quote (hash->list h)))
                                                    ,str
                                                    (with-handlers ([(λ _ #t)
                                                                     (λ (e) `(error ,e))])
                                                      (list 'result ,stx))))))
       (add-stx (datum->syntax stx
                               `(add-piece ,name)))
       (set! names (cons name names))
       (set! eval-res-stx (cons (datum->syntax stx
                                               `(,format-res (cadddr ,name))
                                               stx
                                               stx)
                                eval-res-stx))]

      [`(hidden ,h ,str ,stxs)
       (define name (hash-ref h '#:name))
       (for ([stx stxs]) (add-stx stx))
       (add-stx (datum->syntax stx
                               `(define ,name (list 'hidden
                                                    (make-immutable-hash ,(list 'quote (hash->list h)))
                                                    ,str))))
       (add-stx (datum->syntax stx
                               `(add-piece ,name)))
       (set! names (cons name names))]

      [`(noex ,h ,str)
       (define name (hash-ref h '#:name))
       (add-stx (datum->syntax stx
                               `(define ,name (list 'noex
                                                    (make-immutable-hash ,(list 'quote (hash->list h)))
                                                    ,str))))
       (add-stx (datum->syntax stx
                               `(add-piece ,name)))
       (set! names (cons name names))]))

  (add-stx (datum->syntax stx
                          `(begin
                             (define ,pieces-stx (,get-pieces))
                             (define ,result-name (list ,(list 'quote result-name) meta pieces)))
                          stx
                          stx))
  (set! names (list* pieces-stx result-name names))
  
  `(,(reverse names)
    ,(reverse res-stx)
    ,(reverse eval-res-stx)))


(module+ main

  (require "id-generator.rkt"
           "read-meta.rkt"
           "read-pieces.rkt")

  (define (test-read s)
    (define name #'e)
    (define src #f)
    (define in (open-input-string s))
    (define get-id! (make-id-generator! name))
    (define meta `(meta ,(read-meta in)))
    (define pieces (read-pieces name get-id! src in))
    (assemble-program name get-id! meta pieces))

  (test-read
"



#:meta
hesten: roar
nilsen: knut-benny

et av
snitt

#: meep En tittel
#:hesten: ro #:nilsen to #:kapekatt \"hei på deg\"
en også
to også
"))


   
