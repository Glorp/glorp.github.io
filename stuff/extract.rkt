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
              ,(apply ~a (add-between (apply append (map piece->rkt pieces)) "\n\n")))])]))
     

(define (piece->rkt piece)
  (match piece
    [`(heading ,h ,title)
     '()]
    
    [`(p ,h ,text)
     '()]
    
    [`(top ,h ,str)
     (if (hash-ref h '#:extract #t)
         `(,str)
         '())]
    
    [`(eval ,h ,str ,_)
     (if (hash-ref h '#:extract #t)
         `(,str)
         '())]
    
      [`(hidden ,h ,str)
       (if (hash-ref h '#:extract #f)
           `(,str)
           '())]
    
    [`(noex ,h ,str)
     (if (hash-ref h '#:extract #t)
         `(,str)
         '())]))