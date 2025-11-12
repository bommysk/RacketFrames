;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*    Author: Shubham Kahal
;*    File: tests/boolean-series.rkt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket
(require typed/rackunit)

(require "../data-frame/indexed-series.rkt")
(require "../data-frame/boolean-series.rkt")

; only require bseries functions

; ***********************************************************
; Test Cases
; ***********************************************************

; boolean series tests

; create boolean series
(define series-boolean (new-BSeries (vector #f #t #t #t)
                                    #:index (build-index-from-list (list 'a 'b 'c 'd))))

(bseries-print series-boolean)

(define series-boolean-2 (new-BSeries (vector #f #t #t #f)
                                      #:index (build-index-from-list (list 'a 'b 'c 'd))))

; bseries reference tests
(check-equal? ((bseries-referencer series-boolean) 0) #f)

(check-equal? ((bseries-referencer series-boolean) 1) #t)

(check-equal? (bseries-iref series-boolean (list 0)) (list #f))

(check-equal? (bseries-iref series-boolean (list 1)) (list #t))

(check-equal? (bseries-loc series-boolean 'd) #t)

(check-equal? (bseries-loc series-boolean 'c) #t)

; series length
(check-equal? (bseries-length series-boolean) 4)

; series mapping
(check-equal? (bseries-data (map/bs series-boolean (lambda (b) (if (not b) #t #f)))) (vector #t #f #f #f))

; bseries-loc
(check-equal? (bseries-iloc series-boolean 1) #t)

(check-equal? (bseries-data (assert (bseries-iloc series-boolean (list 2 3)) BSeries?)) (vector #t #t))

(check-equal? (bseries-data (assert (bseries-iloc series-boolean (range 4)) BSeries?)) (vector #f #t #t #t))

(check-equal? (bseries-data series-boolean) (vector #f #t #t #t))

(check-equal? (bseries-loc series-boolean 'a) #f)

(check-equal? (bseries-data (assert (bseries-loc series-boolean (list 'a 'd)) BSeries?)) (vector #f #t))

(check-equal? (bseries-loc-boolean series-boolean (list #t #f #f #f)) #f)

(check-equal? (bseries-data (assert (bseries-loc-boolean series-boolean (list #t #t #f #f)) BSeries?)) (vector #f #t))

(check-equal? (bseries-data (assert (bseries-loc-boolean series-boolean (list #t #f #f #t)) BSeries?)) (vector #f #t))

(check-equal? (bseries-loc series-boolean (list #t #f #f #f)) #f)

(check-equal? (bseries-data (assert (bseries-loc series-boolean (list #t #t #f #f)) BSeries?)) (vector #f #t))

(check-equal? (bseries-data (assert (bseries-loc series-boolean (list #t #f #f #t)) BSeries?)) (vector #f #t))
