;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*    Author: Shubham Kahal
;*    File: tests/boolean-series.rkt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket
(require typed/rackunit)

(require "../data-encode.rkt")


(run-length-encode (list 1 2 3 4 5 5 5 1 2 3 4 4 4 4 4 5 4 5 4 5))

(elem-percent (list 1 2 3 4 5))

(elem-percent (list 1 2 1 1 2 3 3 4 4 4))

(elem-percent (list 'a 'a 'a 'b 'b 'a 'c 'b))

(elem-percent (list 'a 'a 'a 5  7 8 'b 'b 'a 'c 'b 10 10 10 10))

(cdr (last (elem-percent (list 'a 'a 'a 5  7 8 'b 'b 'a 'c 'b 10 10 10 10))))

(max-elem-percent (list 'a 'a 'a 5  7 8 'b 'b 'a 'c 'b 10 10 10 10))

(most-frequent-element-list (list 1 1 1 2 2 4 3 4 1 1))

(most-frequent-element-count (list 1 1 1 2 2 4 3 4 1 1))

(most-frequent-element (list 1 1 1 2 2 4 3 4 1 1))

; ***********************************************************
; Test Cases
; ***********************************************************

(vector-indexes-of-map (vector 1 2 3 4 5 1 1 1 1 2 3 4 4 4 5 6 7 2))
(vector-indexes-of (vector 1 2 3 4 5 1 1 1 1 2 3 4 4 4 5 6 7 2) 1)

(vector-indexes-of-map-set (vector 1 2 3 4 5 1 1 1 1 2 3 4 4 4 5 6 7 2))
(vector-indexes-of-set (vector 1 2 3 4 5 1 1 1 1 2 3 4 4 4 5 6 7 2) 5)

(sequence-indexes-of-map-set (list 1 2 3 4 5 1 1 1 1 2 3 4 4 4 5 6 7 2))
(sequence-indexes-of-set (list 1 2 3 4 5 1 1 1 1 2 3 4 4 4 5 6 7 2) 5)

(sequence-indexes-of-map-set (list 1 2 3 4 5 1 1 'v 1 1 2 3 4 4 4 5 6 7 2 'a 'v 'b 'v))

(sequence-indexes-of-set (list 1 2 3 4 5 1 1 'v 1 1 2 3 4 4 4 5 6 7 2 'a 'v 'b 'v) 'v)