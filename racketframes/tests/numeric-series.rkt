;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*    Author: Shubham Kahal
;*    File: tests/numeric-series.rkt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket
(require typed/rackunit)

(require "../data-frame/indexed-series.rkt")
(require "../data-frame/numeric-series.rkt")
(require "../data-frame/integer-series.rkt")
(require "../data-frame/boolean-series.rkt")
(require racket/flonum racket/unsafe/ops)

; ***********************************************************
; Test Cases
; ***********************************************************
; numeric series tests

(check-equal? (list->flvector (list 1.5 2.5 3.5)) (flvector 1.5 2.5 3.5))

(nseries-data (+/ns/is (new-NSeries (flvector 1.5 2.5 3.5 4.5 5.5)) (new-ISeries (vector 2 2 2 2 2))))

(nseries-data (-/ns/is (new-NSeries (flvector 1.5 2.5 3.5 4.5 5.5)) (new-ISeries (vector 2 2 2 2 2))))

(nseries-data (*/ns/is (new-NSeries (flvector 1.5 2.5 3.5 4.5 5.5)) (new-ISeries (vector 2 2 2 2 2))))

(nseries-data (//ns/is (new-NSeries (flvector 1.5 2.5 3.5 4.5 5.5)) (new-ISeries (vector 2 2 2 2 2))))

(nseries-data (+/is/ns (new-ISeries (vector 2 2 2 2 2)) (new-NSeries (flvector 1.5 2.5 3.5 4.5 5.5))))

(nseries-data (-/is/ns (new-ISeries (vector 2 2 2 2 2)) (new-NSeries (flvector 1.5 2.5 3.5 4.5 5.5))))

(nseries-data (*/is/ns (new-ISeries (vector 2 2 2 2 2)) (new-NSeries (flvector 1.5 2.5 3.5 4.5 5.5))))

(nseries-data (//is/ns (new-ISeries (vector 2 2 2 2 2)) (new-NSeries (flvector 1.5 2.5 3.5 4.5 5.5))))

(bseries-data (>/ns/is (new-NSeries (flvector 1.5 2.5 3.5 4.5 5.5)) (new-ISeries (vector 2 2 2 2 2))))

(bseries-data (</ns/is (new-NSeries (flvector 1.5 2.5 3.5 4.5 5.5)) (new-ISeries (vector 2 2 2 2 2))))

(bseries-data (>=/ns/is (new-NSeries (flvector 1.5 2.5 3.5 4.5 5.5)) (new-ISeries (vector 2 2 2 2 2))))

(bseries-data (<=/ns/is (new-NSeries (flvector 1.5 2.5 3.5 4.5 5.5)) (new-ISeries (vector 2 2 2 2 2))))

(bseries-data (=/ns/is (new-NSeries (flvector 1.5 2.5 3.5 4.5 5.5)) (new-ISeries (vector 2 2 2 2 2))))

(bseries-data (=/ns/is (new-NSeries (flvector 2.0 2.0 2.0 2.0 2.0)) (new-ISeries (vector 2 2 2 2 2))))

(bseries-data (!=/ns/is (new-NSeries (flvector 1.5 2.5 3.5 4.5 5.5)) (new-ISeries (vector 2 2 2 2 2))))

(bseries-data (>/is/ns (new-ISeries (vector 2 2 2 2 2)) (new-NSeries (flvector 1.5 2.5 3.5 4.5 5.5))))

(bseries-data (</is/ns (new-ISeries (vector 2 2 2 2 2)) (new-NSeries (flvector 1.5 2.5 3.5 4.5 5.5))))

(bseries-data (>=/is/ns (new-ISeries (vector 2 2 2 2 2)) (new-NSeries (flvector 1.5 2.5 3.5 4.5 5.5))))

(bseries-data (<=/is/ns (new-ISeries (vector 2 2 2 2 2)) (new-NSeries (flvector 1.5 2.5 3.5 4.5 5.5))))

(bseries-data (=/is/ns (new-ISeries (vector 2 2 2 2 2)) (new-NSeries (flvector 1.5 2.5 3.5 4.5 5.5))))

(bseries-data (=/is/ns (new-ISeries (vector 2 2 2 2 2)) (new-NSeries (flvector 2.0 2.0 2.0 2.0 2.0))))

(bseries-data (!=/is/ns (new-ISeries (vector 2 2 2 2 2)) (new-NSeries (flvector 1.5 2.5 3.5 4.5 5.5))))

; create float series
(define series-float (new-NSeries (flvector 1.5 2.4 3.6 4.1)
                                      #:index (build-index-from-list (list 'a 'b 'c 'd))))

(define series-float-2 (new-NSeries (flvector 5.0 6.0 7.0 8.0)
                                      #:index (build-index-from-list (list 'a 'b 'c 'd))))

; nseries reference tests
(check-equal? ((nseries-referencer series-float) 0) 1.5)

(check-equal? ((nseries-referencer series-float) 1) 2.4)

(check-equal? (nseries-iref series-float (list 0)) (list 1.5))

(check-equal? (nseries-iref series-float (list 1)) (list 2.4))

(check-equal? (nseries-loc series-float 'd) 4.1)

(check-equal? (nseries-loc series-float 'c) 3.6)

; series length
(check-equal? (nseries-length series-float) 4)

; binop 2 series tests
(check-equal? (nseries-data (+/ns series-float series-float-2))
              (flvector 6.5 8.4 10.6 12.1))

(check-equal? (nseries-data (-/ns series-float series-float-2))
              (flvector -3.5 -3.6 -3.4 -3.9000000000000004))

(check-equal? (nseries-data (*/ns series-float series-float-2))
              (flvector 7.5 14.399999999999999 25.2 32.8))

(check-equal? (nseries-data (//ns series-float series-float-2))
              (flvector 0.3 0.39999999999999997 0.5142857142857143 0.5125))

; binop scalar series tests
(check-equal? (nseries-data (+./ns series-float 2.0))
              (flvector 3.5 4.4 5.6 6.1))

(check-equal? (nseries-data (-./ns series-float 1.0))
              (flvector 0.5 1.4 2.6 3.0999999999999996))

(check-equal? (nseries-data (*./ns series-float 2.0))
              (flvector 3.0 4.8 7.2 8.2))

(check-equal? (nseries-data (/./ns series-float 2.0))
              (flvector 0.75 1.2 1.8 2.05))

; map tests
(check-equal? (nseries-data (map/ns series-float (λ: ((x : Float)) (fl+ x 1.0))))
              (flvector 2.5 3.4 4.6 5.1))

; agg tests
(check-equal? (apply-agg-ns 'sum series-float) 11.6)

(check-equal? (apply-agg-ns 'mean series-float) 2.9)

(check-equal? (apply-agg-ns 'count series-float) 4)

(apply-agg-ns 'median series-float)

(apply-agg-ns 'mode series-float) 

; statistics tests
(check-equal? (apply-stat-ns 'variance series-float) 1.035)

(check-equal? (apply-stat-ns 'stddev series-float) 1.0173494974687902)

(check-equal? (apply-stat-ns 'skewness series-float) -0.18946647505895)

(nseries-print series-float)

(apply-agg-ns 'mean series-float)

(apply-agg-ns 'median series-float)

(apply-agg-ns 'mode series-float)

(define series-float-3 (new-NSeries (flvector 1.5 2.4 5.89 3.6 4.1 4.1)
                                      #:index (build-index-from-list (list 'a 'a 'c 'd 'e 'a))))

(nseries-print series-float-3)

(apply-agg-ns 'mean series-float-3)

(apply-agg-ns 'median series-float-3)

(apply-agg-ns 'mode series-float-3)