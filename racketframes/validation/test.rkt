#lang typed/racket

(require RacketFrames)

(require "../../../util/list.rkt")

(define now current-inexact-milliseconds)

(define level1 (build-index-from-list (list 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j)))

;(random-number-list 10000 200)

;(random-symbol-list 50 10 "" '())

(define-predicate ListofFixnum? (Listof Fixnum))

(define integer-series-1 (new-ISeries (list->vector (assert (random-number-list 2000000 200) ListofFixnum?))))

(define integer-series-2 (new-ISeries (list->vector (assert (random-number-list 2000000 200) ListofFixnum?))))

(define series-comp-<-bench-before (now))
(</is integer-series-1 integer-series-2)