#lang setup/infotab

(define collection 'multi)

(define deps (list "base"
                   "math-lib"
                   "plot-gui-lib"
                   "plot-lib"
                   "rackunit-typed"
                   "typed-racket-lib"
                   "typed-racket-more"))

(define build-deps (list "racket-doc"
                         "scribble-lib"))
