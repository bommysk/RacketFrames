#lang typed/racket

(require typed/rackunit)
(require racket/fixnum)

(require "../data-frame/boolean-series.rkt")
(require "../data-frame/integer-series.rkt")
(require "../data-frame/integer-series-ops.rkt")

; create integer series
(define series-integer (new-ISeries (fxvector 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)))

(check-equal? (iseries-data (iseries-unique series-integer)) #(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))

(check-equal? (iseries-data (iseries-head series-integer)) #(1 2 3 4 5 6 7 8 9 10))

(define series-integer-duplicates (new-ISeries (fxvector 5 5 5 5 5)))

(check-equal? (iseries-data (iseries-unique series-integer-duplicates)) #(5))

(check-equal? (iseries-data (iseries-head series-integer-duplicates)) #(5 5 5 5 5))

(define series-integer-negatives (new-ISeries (fxvector -5 -5 -5 -5 -5)))

(check-equal? (iseries-data (iseries-abs series-integer-negatives)) #(5 5 5 5 5))

(check-equal? (iseries-data (iseries-isna series-integer)) #())

;(define vec : (Vectorof Fixnum) (vector 1 2 3 4 4 3 2 1 0 0 1 2 3 8 7 6 3 90 23))
;(remove-duplicates-sequence vec)
