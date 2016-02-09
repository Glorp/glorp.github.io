#lang racket

(require (only-in xml xexpr->string))
(require "images.rkt"
         "git-halp.rkt"
         (only-in racket/path find-relative-path))

(define-syntax-rule (with-anchor [a hash] body ...)
  (let ([a `(a ((name ,(symbol->string (hash-ref hash '#:name)))))])
    body
    ...))

(define (get-par-text text)
  (for/list ([t text])
    (match t
      [`(norm ,s) s]
      [`(em ,s) `(em ,s)]
      [`(strong ,s) `(strong ,s)]
      [`(tt ,s) `(span ([class "code"]) ,s)]
      [`(link ,url ,text) `(a ([class "link"] [href ,url]) ,text)])))

(define (meta->xexprs meta)
  (define title (hash-ref meta 'title "untitled"))
  `((title ,title)
    (link ((rel "stylesheet") (href "styles.css")))))

(define ((piece->xexpr base-name) piece)
  (match piece
    [`(heading ,h ,title)
     (define name (symbol->string (hash-ref h '#:name)))
     (define hx (match (hash-ref h '#:level)
                  [0 'h1]
                  [1 'h2]
                  [2 'h3]))
     `(,hx (a ((name ,name))) (a ((href ,(~a "#" name))) ,title))]
    
    [`(p ,h ,text)
     (with-anchor [a h]
                  `(p ((class "par")) ,a ,@(get-par-text text)))]
    
    [`(top ,h ,str)
     (with-anchor [a h]
                  `(pre ,a ,str))]
    
    [`(eval ,h ,str (result ,res))
     (with-anchor [a h]
                  `(pre ,a "> " ,str "\n" ,@(format-res (pretty-format res) base-name)))]

    [`(eval ,h ,str (error ,e))
     (with-anchor [a h]
                  `(pre ,a "> " ,str "\n" (span ([class "error"]) ,(exn-message e))))]
    
      [`(hidden ,h ,str)
       #f]
    
    [`(noex ,h ,str)
     (with-anchor [a h]
                  `(pre ,a ,str))]))

(define (format-res str base-name)
  (define s1 "\"#<img:")
  (define s2 ">\"")
  (cond [(string-prefix? str s1)
         `((img ((src ,(~a base-name "/pict/" (car (string-split (car (string-split str s1)) s2)))))))]
        [else
         (define bits (string-split str s1))
         (define img-pairs (map (λ (x) (string-split x s2)) (cdr bits)))
         `(,(car bits)
           ,@(apply append (map (λ (x)
                                  `((img ((src ,(~a base-name "/pict/" (car x)))))
                                    ,(cadr x)))
                                img-pairs)))]))

(define git-root
  (let ()
    (define-values  (git-root blah bloh) (split-path (syntax-source #'e)))
    git-root))

(define repo-url "https://github.com/Glorp/glorp.github.io")

(define (commit->url commit path)
  (define p (if (path? path)
                (path->string path)
                p))
  (format "~a/blob/~a/~a" repo-url commit (string-replace p "\\" "/")))

(define (info meta)
  (define path (hash-ref meta 'file))
  (define-values (base file foo) (split-path path))
  (define rel (find-relative-path git-root path))
  (define gitinfo
    (match (git-info git-root rel)
      ['untracked "nope"]
      [`(commit ,c) `(a ((href ,(commit->url c rel))) ,(path->string file) (br) ,c)]
      [`(modified ,c) `(a ((href ,(commit->url c rel))) ,(path->string file) (br) ,c)]))
    
  `(div ((class "info"))
        "Generated from:"
        (br)
        ,gitinfo))

(define nav
  '(div ((class "nav"))
        (a ((href "top.html"))
           (div ((class "navlink")) "⊤"))
        " "
        (a ((href "think.html"))
           (div ((class "navlinkf")) "thinkpieces"))
        " "
        (a ((href "about.html"))
           (div ((class "navlink")) "about"))
        " "
        (a ((href "bot.html"))
           (div ((class "navlink")) "⊥"))))

(define (stuff->path/xexpr stuff)
  (match stuff
    [`(,name ,meta ,pieces)
     (define rktfile (string->path (hash-ref meta 'file)))
     (define-values (dir filename _) (split-path rktfile))
     (define base-name (path->string (path-replace-suffix filename "")))
     (define html-file (path-replace-suffix rktfile ".html"))
     (define mydir (build-path dir base-name))
     (define pict-dir (build-path mydir "pict"))
     (unless (directory-exists? mydir)
       (make-directory mydir))
     (when (directory-exists? pict-dir)
       (for/list ([f (in-directory pict-dir)])
         (delete-file f))
       (delete-directory pict-dir))
     (make-directory pict-dir)

     (match-define (list make-pretty-print-print-hook make-pretty-print-size-hook) (make-image-printer pict-dir))

     (parameterize ([pretty-print-print-hook (make-pretty-print-print-hook (pretty-print-print-hook))]
                    [pretty-print-size-hook (make-pretty-print-size-hook (pretty-print-size-hook))])
     
       (list html-file
             `(html (head (meta ((charset "UTF-8")) ,@(meta->xexprs meta)))
              (body (div ((class "content"))
                         (div ((class "navinfo"))
                              ,nav
                              ,(info meta))
                         (div ((class "text"))
                              ,@(filter (λ (x) x) (map (piece->xexpr base-name) pieces))))))))]))

(define (->html xexpr)
  (match-define (list make-pretty-print-print-hook make-pretty-print-size-hook) (make-image-printer "asd/qwe"))
  (string-append "<!DOCTYPE HTML>\n"
                 (xexpr->string xexpr)))

(define (write-file stuff)
  (match (stuff->path/xexpr stuff)
    [`(,file ,xexpr)
     (call-with-output-file file #:exists 'truncate
       (λ (port)
         (display (->html xexpr) port)))]))
