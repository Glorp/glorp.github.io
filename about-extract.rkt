#lang racket

(require 2htdp/image
         lang/posn)

(define (filled f fill . args)
  (define pen (make-pen 'black 3 'solid 'round 'round))
  (overlay (apply f `(,@args outline ,pen))
           (apply f `(,@args solid ,fill))))

(filled circle 'red 20)

(define bottom
  (filled polygon 'brown (list (make-posn 0 0)
                               (make-posn 200 0)
                               (make-posn 180 50)
                               (make-posn 30 50))))

bottom

(define top (above/align 'right
                         (filled rectangle
                                 'darkgray
                                 20
                                 23)
                         (filled rectangle
                                 'red
                                 70
                                 50)))

top

(define boat (above top bottom))

(ellipse 70 40 'solid 'blue)

(define blue (ellipse 70 40 'solid 'blue))

(define sea (foldr (λ (_ img) (overlay/xy blue 50 0 img))
                   blue
                   '(1 2 3 4)))

(crop 10 0 250 130 (underlay/xy boat -40 95 sea))