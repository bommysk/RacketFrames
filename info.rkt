#lang setup/infotab

(define collection 'multi)

(define deps (list "scribble-lib"
                   "base"
                   "math-lib"
                   "plot-gui-lib"
                   "plot-lib"
                   "rackunit-typed"
                   "typed-racket-lib"
                   "typed-racket-more"))

(define build-deps (list "typed-racket-doc"
                         "racket-doc"
                         "scribble-lib"))
