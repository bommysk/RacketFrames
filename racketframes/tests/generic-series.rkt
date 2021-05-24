;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*    Author: Shubham Kahal
;*    File: boolean-series.rkt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket
(require typed/rackunit)

(require "../data-frame/indexed-series.rkt")
(require "../data-frame/generic-series.rkt")

; create generic series
(define series-generic (new-GenSeries (vector 1 2.5 'categorical #t)
                                      #:index (build-index-from-list (list 'a 'b 'c 'd))))

(define series-generic-2 (new-GenSeries (vector 5 6 7 8)
                                      #:index (build-index-from-list (list 'a 'b 'c 'd))))

; gen-series reference tests
(check-equal? ((gen-series-referencer series-generic) 0) 1)

(check-equal? ((gen-series-referencer series-generic) 1) 2.5)

(check-equal? (gen-series-iref series-generic (list 0)) (list 1))

(check-equal? (gen-series-iref series-generic (list 1)) (list 2.5))

(check-equal? (gen-series-label-ref series-generic 'd) (list #t))

(check-equal? (gen-series-label-ref series-generic 'c) (list 'categorical))

; gen-series length
(check-equal? (gen-series-length series-generic) 4)

; point struct
(struct point ([x : Integer] [y : Integer]) #:transparent)

; create point struct series
(define gen-series-point (new-GenSeries (vector (point 1 2) (point 3 4) (point 5 6) (point 7 8) (point 9 10)) #:index (build-index-from-list (list 'a 'b 'c 'd 'e))))

(check-equal? (gen-series-data gen-series-point) (vector (point 1 2) (point 3 4) (point 5 6) (point 7 8) (point 9 10)))

; point series ref by index
(check-equal? (gen-series-iref gen-series-point (list 2)) (list (point 5 6)))

; point series ref by label
(check-equal? (gen-series-label-ref gen-series-point 'd) (list (point 7 8)))

(define multi-index-gen-series (new-GenSeries (vector "hello moto" 2 3 4 5) #:index (build-multi-index-from-list (list (list 'a 'b 'c 'd 'e) (list 1 2 3 4 3) (list 5 6 7 8 9)))))

(define multi-index-gen-series-2 (new-GenSeries (vector "hello moto" 2 3 4 "robo") #:index (build-multi-index-from-list (list (list 'a 'b 'c 'a 'c) (list 1 2 3 4 3) (list 5 6 7 8 9)))))

(check-equal? (gen-series-loc-multi-index multi-index-gen-series (list "a" "1" "5")) "hello moto")

(check-equal? (gen-series-loc-multi-index multi-index-gen-series-2 (list "c" "3" "9")) "robo")
