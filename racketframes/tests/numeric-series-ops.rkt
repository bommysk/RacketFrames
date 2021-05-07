;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*    Author: Shubham Kahal
;*    File: tests/numeric-series-ops.rkt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket
(require typed/rackunit)

(require "../main.rkt")

; create integer series
(define series-float (new-NSeries (flvector 1.5 2.5 3.5 4.5 5.5 6.5 7.5 8.5 9.5 10.5 11.5 12.5 13.5 14.5 15.5 16.5 17.5 18.5 19.5 20.5) #f))

(check-equal? (nseries-data (nseries-unique series-float))
              (flvector 1.5 2.5 3.5 4.5 5.5 6.5 7.5 8.5 9.5 10.5 11.5 12.5 13.5 14.5 15.5 16.5 17.5 18.5 19.5 20.5))
              

(check-equal? (nseries-data (nseries-head series-float))
              (flvector 1.5 2.5 3.5 4.5 5.5 6.5 7.5 8.5 9.5 10.5))

(define series-float-duplicates (new-NSeries (flvector 5.0 5.0 5.0 5.0 5.0) #f))

(check-equal? (nseries-data (nseries-unique series-float-duplicates)) (flvector 5.0))

(check-equal? (nseries-data (nseries-head series-float-duplicates))
              (flvector 5.0 5.0 5.0 5.0 5.0))

(define series-float-negatives (new-NSeries (flvector -5.0 -5.0 -5.0 -5.0 -5.0) #f))

(check-equal? (nseries-data (nseries-abs series-float-negatives))
              (flvector 5.0 5.0 5.0 5.0 5.0))

;(bseries-data (nseries-isna series-float))
