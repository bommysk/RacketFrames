;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*    Author: Shubham Kahal
;*    File: datetime-series.rkt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket
(require typed/rackunit)
(require racket/unsafe/ops)

(require
  (only-in racket/flonum
           make-flvector flvector? flvector
           flvector-ref flvector-set!
           flvector-length)
  (only-in racket/set
           set-empty? set-member?
           list->set))

(require "../data-frame/indexed-series.rkt")
(require "../data-frame/datetime-series.rkt")
(require   "../util/datetime.rkt")

; ***********************************************************
; Test Cases
; ***********************************************************

; integer series tests

; create integer series
(define series-datetime (new-DatetimeSeries (vector (Datetime (Date 1 1 1) (Time 1 1 1 1 1)))
                                      #:index (build-index-from-list (list 'a))))

(define series-datetime-2 (new-DatetimeSeries (vector (Datetime (Date 1 1 1) (Time 1 1 1 1 1)) (Datetime (Date 2 2 2) (Time 2 2 2 2 2)) (Datetime (Date 3 3 3) (Time 3 3 3 3 3)) (Datetime (Date 4 4 4) (Time 4 4 4 4 4)))
                                      #:index (build-index-from-list (list 'a 'b 'c 'd))))

; iseries reference tests
(check-equal? ((datetime-series-referencer series-datetime) 0) (Datetime (Date 1 1 1) (Time 1 1 1 1 1)))

(check-equal? ((datetime-series-referencer series-datetime-2) 1) (Datetime (Date 2 2 2) (Time 2 2 2 2 2)))

(check-equal? (datetime-series-iloc series-datetime 0) (Datetime (Date 1 1 1) (Time 1 1 1 1 1)))

(check-equal? (datetime-series-iloc series-datetime-2 1) (Datetime (Date 2 2 2) (Time 2 2 2 2 2)))

(check-equal? (datetime-series-loc series-datetime-2 'd) (Datetime (Date 4 4 4) (Time 4 4 4 4 4)))

(check-equal? (datetime-series-loc series-datetime-2 'c) (Datetime (Date 3 3 3) (Time 3 3 3 3 3)))

; series length
(check-equal? (datetime-series-length series-datetime) 1)
(check-equal? (datetime-series-length series-datetime-2) 4)

; map tests
( map/datetime-series-data series-datetime (λ: ((x : Datetime)) (begin (displayln (~a x)) x)))
;(check-equal? (datetime-series-data ( map/datetime-series-data series-datetime (λ: ((x : Datetime)) (date-to-seconds x))))
 ;             (vector 2 3 4 5))

