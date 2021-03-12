;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*    Author: Shubham Kahal
;*    File: tests/indexed-series.rkt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket
(require typed/rackunit)


(require "../data-frame/indexed-series.rkt")
(require "../data-frame/generic-series.rkt")

; ***********************************************************
; Test Cases
; ***********************************************************

; Not sure how to check
;(check-equal? (build-index-from-list (list 'a 'b 'c 'd))
;              (hash 'b 1 'c' 2 'd 3 'a 0))

(index-keys (build-index-from-list (list 'a 'b 'c 'd)))
(index-idxes(build-index-from-list (list 'a 'b 'c 'd)))

(index-keys-intersection (list (build-index-from-list (list 'a 'b 'c 'd)) (build-index-from-list (list 'a 'b 'c 'd)) (build-index-from-list (list 'a 'b 'c 'd))))

(index-keys-intersection (list (build-index-from-list (list 'a 'b 'c 'd)) (build-index-from-list (list 'a 'b 'z 'd)) (build-index-from-list (list 'k 'l 'c 'd))))

; checks numerical index of label
(check-equal? (get-index (build-index-from-list (list 'a 'b 'c 'd)) 'a) (list 0))
(check-equal? (get-index (build-index-from-list (list 'a 'b 'c 'd)) 'c) (list 2))

; indexing returns a list because we can have the same index values
; checks numerical idx of LabelIndex
(check-equal? (label->lst-idx (assert (build-index-from-list (list 'a 'b 'c 'd)) LabelIndex?) 'd)  (list 3))
(check-equal? (label->lst-idx (assert (build-index-from-list (list 'a 'b 'c 'd)) LabelIndex?) 'c) (list 2))

; checks numerical idx of LabelIndex
(check-equal? (idx->label (assert (build-index-from-list (list 'a 'b 'c 'd)) LabelIndex?) 3) 'd)
(check-equal? (idx->label (assert (build-index-from-list (list 'a 'b 'c 'd)) LabelIndex?) 2) 'c)

; checks to see if we have a labelled index
(check-equal? (is-labeled? (assert (build-index-from-list (list 'a 'b 'c 'd)) LabelIndex?)) #t)

(define g-series-integer-not-labeled (new-GenSeries (vector 1 2 3 4) #f))

;(check-equal? (is-labeled? (gen-series-index g-series-integer)) #f)

; generic series tests

; create integer series
(define g-series-integer (new-GenSeries (vector 1 2 3 4) (build-index-from-list (list 'a 'b 'c 'd))))

; expect exception
;(new-GenSeries (vector 1 2 3 4 5) (build-index-from-list (list 'a 'b 'c 'd 'a)))

(define g-series-integer-2 (new-GenSeries (vector 1 2 3 4 5) (build-index-from-list (list 'a 'b 'c 'd 'e))))

; create float series
(define g-series-float (new-GenSeries (vector 1.5 2.5 3.5 4.5 5.5) (build-index-from-list (list 'a 'b 'c 'd 'e))))

; create symbol series
(define g-series-symbol (new-GenSeries (vector 'e 'f 'g 'h) (list 'a 'b 'c 'd)))

; point struct
(struct point ([x : Integer] [y : Integer]) #:transparent)

; create point struct series
(define g-series-point (new-GenSeries (vector (point 1 2) (point 3 4) (point 5 6) (point 7 8) (point 9 10)) (build-index-from-list (list 'a 'b 'c 'd 'e))))

;(gen-series-data g-series-point)

; point series ref by index
;(gen-series-iref g-series-point (list 2))

; point series ref by label
;(series-ref g-series-point 'd)

;(gen-series-data (map/GSeries g-series-point (λ: ((p : point))
;                                (point-x p))))

; integer series ref by index
(check-equal? (gen-series-iref g-series-integer (list 2)) (list 3))

; integer series ref by label
(check-equal? (gen-series-loc g-series-integer 'd) 4)

(check-equal? (gen-series-loc g-series-integer-2 'a) 1)

; symbol series ref by index
(check-equal? (gen-series-iref g-series-symbol (list 2)) (list 'g))

; symbol series ref by label
(check-equal? (gen-series-loc g-series-symbol 'd) 'h)

; series length
(check-equal? (gen-series-length g-series-symbol) 4)

; series map
; unknown type error
;(check-equal? (gen-series-data (map/gen-s g-series-integer (λ: ((x : Integer))
;                                                            (add1 x)))) #(2 3 4 5))
; create struct series
(define g-series-struct (new-GenSeries (vector (list #f) (list #f)) (build-index-from-list (list 'a 'b))))

; struct series ref by index
;(check-equal? (LabelIndex-index (assert (gen-series-iref g-series-struct 1) LabelIndex?)) (LabelIndex-index (LabelIndex #f)))

; integer series ref by label
; checks labeling function which converts labels hash to list
;(check-equal? (labeling (assert (build-index-from-list (list 'a 'b 'c 'd)) LabelIndex?))
 ;             '((b 1) (c 2) (d 3) (a 0)))

; checks label sorting
(check-equal? (label-sort-lexical (assert (build-index-from-list (list 'b 'd 'a 'c)) LabelIndex?))
              '((a 2) (b 0) (c 3) (d 1)))

; check label sorting by position
; (check-equal? (label-sort-positional (LabelIndex (build-index-from-list (list 'b 'd 'a 'c))))
;              '((b . 0) (d . 1) (a . 2) (c . 3)))
