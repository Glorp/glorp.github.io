#lang racket

(require "stuff/stuff.rkt")

(require "about.rkt"
         "index.rkt"
         "top.rkt"
         "think.rkt")

(write-files index)
(write-files top)
(write-files about)
(write-files think)
