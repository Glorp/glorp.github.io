#lang racket
(provide piece->rkt
         extract-rkt)

(define (extract-rkt stuff)
  (match stuff
    [`(,name ,meta ,pieces)
     (cond [(not (hash-ref meta 'extract #f))
            #f]
           [else
            (define-values (dir filename _) (split-path (hash-ref meta 'file)))
            (define newfile (build-path dir (~a (path-replace-suffix filename "") "-extract.rkt")))
            `(,newfile
              ,(apply ~a (add-between (apply append (map piece->rkt pieces)) "\n")))])]))
     

(define (piece->rkt piece)
  (match piece
    [`(heading ,h ,title)
     '()]
    
    [`(p ,h ,text)
     '()]
    
    [`(top ,h ,str)
     (if (hash-ref h '#:extract #t)
         `(,(~a str "\n"))
         '())]
    
    [`(eval ,h ,str ,_)
     (if (hash-ref h '#:extract #t)
         `(,(~a str "\n"))
         '())]
    
      [`(hidden ,h ,str)
       (if (hash-ref h '#:extract #f)
           `(,(~a str "\n"))
           '())]
    
    [`(noex ,h ,str)
     (if (hash-ref h '#:extract #t)
         `(,(~a str "\n"))
         '())]))