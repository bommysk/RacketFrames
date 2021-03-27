#lang typed/racket

(require typed/rackunit)

(require "../data-frame/boolean-series.rkt")
(require "../data-frame/integer-series.rkt")
(require "../data-frame/integer-series-ops.rkt")

; create integer series
(define series-integer (new-ISeries (vector 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20) #f))

(iseries-data (iseries-unique series-integer))

(iseries-data (iseries-head series-integer))

(define series-integer-duplicates (new-ISeries (vector 5 5 5 5 5) #f))

(iseries-data (iseries-unique series-integer-duplicates))

(iseries-data (iseries-head series-integer-duplicates))

(define series-integer-negatives (new-ISeries (vector -5 -5 -5 -5 -5) #f))

(iseries-data (iseries-abs series-integer-negatives))

(bseries-data (iseries-isna series-integer))

;(define vec : (Vectorof Fixnum) (vector 1 2 3 4 4 3 2 1 0 0 1 2 3 8 7 6 3 90 23))
;(remove-duplicates-sequence vec)
