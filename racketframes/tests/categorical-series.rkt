#lang typed/racket

(require typed/rackunit)

(require "../data-frame/indexed-series.rkt")
(require "../data-frame/categorical-series.rkt")

; ***********************************************************
; Test Cases
; ***********************************************************

; categorical series tests

; create categorical series
(define series-categorical (new-CSeries (vector 'a 'b 'c 'd)
                                      #:index (build-index-from-list (list 'e 'f 'g 'h))))

(define series-categorical-2 (new-CSeries (vector 'billy 'bob 'kristina 'edan 'joe)
                                      #:index (build-index-from-list (list 5 4 3 2 1))))

; iseries reference tests
(check-equal? ((cseries-referencer series-categorical) 0) 'a)

(check-equal? ((cseries-referencer series-categorical) 1) 'b)

(check-equal? (cseries-iref series-categorical (list 0)) (list 'a))

(check-equal? (cseries-iref series-categorical (list 1)) (list 'b))

(check-equal? (cseries-iloc series-categorical-2 4) 'joe)

(check-equal? (cseries-iloc series-categorical-2 2) 'kristina)

(check-equal? (cseries-range series-categorical 2) (vector 'a 'b))

(check-equal? (cseries-range series-categorical 1) (vector 'a))

(check-equal? (cseries-loc-boolean series-categorical (list #f #f #f #t)) 'd)

(check-equal? (cseries-data (assert (cseries-loc-boolean series-categorical (list #f #f #t #t)) CSeries?)) (vector 'c 'd))
