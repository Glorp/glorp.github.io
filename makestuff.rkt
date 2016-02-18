#lang racket

(require "stuff/stuff.rkt")

(module+ main
  (require "about.rkt"
           "index.rkt"
           "top.rkt"
           "think.rkt")

  (write-file index)
  (write-file top)
  (write-file about)
  (write-file think))