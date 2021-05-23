;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*    Author: Shubham Kahal
;*    File: tests/integer-series.rkt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket
(require typed/rackunit)

(require "../data-frame/indexed-series.rkt")
(require "../data-frame/integer-series.rkt")
(require racket/unsafe/ops)
(require racket/fixnum)

; ***********************************************************
; Test Cases
; ***********************************************************

; integer series tests

; create integer series
(define vec : (Vectorof Fixnum) (vector 1 2 3 4))

(define series-integer : ISeries (new-ISeries vec
                                              #:index (build-index-from-list (list 'a 'b 'c 'd))))

(iseries-print series-integer)

(define series-integer-2 : ISeries (new-ISeries (list 5 6 7 8)
                                                #:index (build-index-from-list (list 'a 'b 'c 'd))))

(iseries-print series-integer-2)

(define series-integer-3 : ISeries (new-ISeries (list 5 5 6 7 8)
                                                #:index (build-index-from-list (list 'a 'a 'b 'c 'd))))

(define series-fxvector : ISeries (new-ISeries (fxvector 1 2 3 4)
                                              #:index (build-index-from-list (list 'fxa 'fxb 'fxc 'fxd))))

(iseries-print series-fxvector)

(extract-index (build-index-from-list (list 'a 'b 'c 'a 'd)))

(check-equal? (iseries-data (assert (iseries-loc series-integer-3 'a) ISeries?)) (vector 5 5))

; iseries reference tests
(check-equal? ((iseries-referencer series-integer) 0) 1)

(check-equal? ((iseries-referencer series-integer) 1) 2)

(check-equal? (iseries-iref series-integer (list 0)) (list 1))

(check-equal? (iseries-data (assert (iseries-iloc series-integer (list 0)) ISeries?))
              (vector 1))

(check-equal? (iseries-iref series-integer (list 1)) (list 2))

(check-equal? (iseries-iloc series-integer 1) 2)

(check-equal? (iseries-loc series-integer 'd) 4)

(check-equal? (iseries-loc series-integer 'c) 3)

; series length
(check-equal? (iseries-length series-integer) 4)

; binop 2 series tests
(check-equal? (iseries-data (+/is series-integer series-integer-2))
              (vector 6 8 10 12))

(check-equal? (iseries-data (-/is series-integer series-integer-2))
              (vector -4 -4 -4 -4))

(check-equal? (iseries-data (*/is series-integer series-integer-2))
              (vector 5 12 21 32))

; currently doing only integer division
(check-equal? (iseries-data (//is series-integer series-integer-2))
              (vector 0 0 0 0))

(check-equal? (iseries-data (r/is series-integer series-integer-2))
              (vector 1 2 3 4))

(check-equal? (iseries-data (%/is series-integer series-integer-2))
              (vector 1 2 3 4))

; binop scalar series tests
(check-equal? (iseries-data (+./is series-integer 2))
              (vector 3 4 5 6))

(check-equal? (iseries-data (-./is series-integer 1))
              (vector 0 1 2 3))

(check-equal? (iseries-data (*./is series-integer 2))
              (vector 2 4 6 8))

(check-equal? (iseries-data (/./is series-integer 2))
              (vector 0 1 1 2))

(check-equal? (iseries-data (r./is series-integer 2))
              (vector 1 0 1 0))

(check-equal? (iseries-data (%./is series-integer 2))
              (vector 1 0 1 0))

; map tests
(check-equal? (iseries-data (map/is series-integer (λ: ((x : Fixnum)) (unsafe-fx+ x 1))))
              (vector 2 3 4 5))

; iseries filter
(check-equal? (iseries-data (iseries-filter (iseries-notna series-integer) (lambda ((val : RFFixnum)) (even? (assert val fixnum?))))) (vector 2 4))

(check-equal? (iseries-data (iseries-filter series-integer (lambda ((val : RFFixnum)) (odd? (assert val fixnum?))))) (vector 1 3))

(check-equal? (iseries-data (iseries-filter-not series-integer (lambda ((val : RFFixnum)) (even? (assert val fixnum?))))) (vector 1 3))

(check-equal? (iseries-data (iseries-filter-not series-integer (lambda ((val : RFFixnum)) (odd? (assert val fixnum?))))) (vector 2 4))

; agg tests
(check-equal? (apply-agg-is 'sum series-integer) 10)

(check-equal? (apply-agg-is 'mean series-integer) 10/4)

(check-equal? (apply-agg-is 'count series-integer) 4)

(check-equal? (apply-agg-is 'min series-integer) 1)

(check-equal? (apply-agg-is 'max series-integer) 4)

; statistics tests
(check-equal? (apply-stat-is 'variance series-integer) 5/4)

(check-equal? (apply-stat-is 'stddev series-integer) 1.118033988749895)

(check-equal? (apply-stat-is 'skewness series-integer) 0.0)

; iseries print
(iseries-print (assert (iseries-iloc series-integer (list 1 3)) ISeries?))

(iseries-print series-integer)

(LabelIndex-index (build-multi-index-from-list (list (list 'a 'b 'c 'a 'c) (list 1 2 3 4 3))))

; iseries multi-index
(define multi-index-iseries : ISeries (assert (new-ISeries (list 1 2 3 4 5) #:index (build-multi-index-from-list (list (list 'a 'b 'c 'a 'c) (list 1 2 3 4 5)))) ISeries?))

(define multi-index-iseries-2 : ISeries (assert (new-ISeries (list 500 2 3 4 100) #:index (build-multi-index-from-list (list (list 'a 'b 'c 'a 'c) (list 1 2 3 4 3) (list 5 6 7 8 9)))) ISeries?))

(define multi-index-iseries-3 : ISeries (assert (new-ISeries (list 100 200 300 400 500) #:index (build-multi-index-from-list (list (list 'a 'b 'c 'a 'c) (list 1 2 3 4 3)))) ISeries?))

(check-equal? (iseries-loc-multi-index multi-index-iseries (list "a" "1")) 1)

(check-equal? (iseries-loc-multi-index multi-index-iseries-2 (list "c" "3" "9")) 100)

(iseries-print (assert (iseries-loc-multi-index multi-index-iseries-3 (list "c" "3")) ISeries?))

(define int-vector : (Vectorof RFFixnum) (vector 0 1 0 1 0 1 0 0 0 0 1 1 1 0 1))
(new-ISeries int-vector)

(iseries-print (new-ISeries int-vector #:index (build-index-from-list (range (vector-length int-vector)))))

(define iseries-with-nan : ISeries (new-ISeries int-vector #:index (build-index-from-list (range (vector-length int-vector))) #:fill-null 'NaN))
(iseries-print iseries-with-nan)
(iseries-null-value iseries-with-nan)
(iseries-custom-null-value iseries-with-nan)

(set-ISeries-fixnum-null-value-inplace iseries-with-nan 1)
(define iseries-with-fixnum-null-value : ISeries iseries-with-nan)
(iseries-print iseries-with-fixnum-null-value)
(iseries-null-value iseries-with-fixnum-null-value)
(iseries-custom-null-value iseries-with-fixnum-null-value)

; ISeries Nominals Tests
; (new-ISeries-Nominals (make-RFFixnum-vector (list 1 2 98228012 3 4 5 98228012 6 7 8999 98228012 98228012)) #f)

(iseries-print (new-ISeries int-vector #:index (build-index-from-list (range (vector-length int-vector)))))