#lang racket
(require racket/file file/convertible racket/pretty)
(provide make-image-printer)

(define (make-image-printer dir)

  (define (save-tmpimage imgbytes i)
    (define filename (format "pict~a.png" i))
    (define path (build-path dir filename))
    (with-output-to-file path #:exists 'truncate
      (lambda () (display imgbytes)))
    (format "#<img:~a>" filename))
  
  (define img-cache '())

  (define (cache-img img)
    (cond [(memf (λ (x) (equal? (cadr x) img)) img-cache) => (λ (l) (caar l))]
          [else (define str (save-tmpimage (convert img 'png-bytes) (length img-cache)))
                (set! img-cache (cons (list str img) img-cache))
                str]))

  (define (image-value value)
    (and (convertible? value)
         (cache-img value)))

  (define (make-pretty-print-size-hook [orig (pretty-print-size-hook)])
    (lambda (value display? port)
      (if (convertible? value)
          (pretty-print-columns)
          (orig value display? port))))
  
  (define (make-pretty-print-print-hook [orig (pretty-print-print-hook)])
    (lambda (value display? port)
      (let [(img (image-value value))]
        (if img (print img port) (orig value display? port)))))

  (list make-pretty-print-print-hook make-pretty-print-size-hook))
  
