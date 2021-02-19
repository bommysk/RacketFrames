;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*    Author: Shubham Kahal
;*    File: tests/integer-series.rkt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket
(require typed/rackunit)

(require "../data-frame/indexed-series.rkt")
(require "../data-frame/integer-series.rkt")
(require racket/unsafe/ops)

; ***********************************************************
; Test Cases
; ***********************************************************

; integer series tests

; create integer series
(define series-integer (new-ISeries (vector 1 2 3 4)
                                      (build-index-from-list (list 'a 'b 'c 'd))))

(define series-integer-2 (new-ISeries (vector 5 6 7 8)
                                      (build-index-from-list (list 'a 'b 'c 'd))))

(define series-integer-3 (new-ISeries (vector 5 5 6 7 8)
                                      (build-index-from-list (list 'a 'a 'b 'c 'd))))

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
(check-equal? (ISeries-data (+/is series-integer series-integer-2))
              (vector 6 8 10 12))

(check-equal? (ISeries-data (-/is series-integer series-integer-2))
              (vector -4 -4 -4 -4))

(check-equal? (ISeries-data (*/is series-integer series-integer-2))
              (vector 5 12 21 32))

; currently doing only integer division
(check-equal? (ISeries-data (//is series-integer series-integer-2))
              (vector 0 0 0 0))

(check-equal? (ISeries-data (r/is series-integer series-integer-2))
              (vector 1 2 3 4))

(check-equal? (ISeries-data (%/is series-integer series-integer-2))
              (vector 1 2 3 4))

; binop scalar series tests
(check-equal? (ISeries-data (+./is series-integer 2))
              (vector 3 4 5 6))

(check-equal? (ISeries-data (-./is series-integer 1))
              (vector 0 1 2 3))

(check-equal? (ISeries-data (*./is series-integer 2))
              (vector 2 4 6 8))

(check-equal? (ISeries-data (/./is series-integer 2))
              (vector 0 1 1 2))

(check-equal? (ISeries-data (r./is series-integer 2))
              (vector 1 0 1 0))

(check-equal? (ISeries-data (%./is series-integer 2))
              (vector 1 0 1 0))

; map tests
(check-equal? (ISeries-data (map/is series-integer (λ: ((x : Fixnum)) (unsafe-fx+ x 1))))
              (vector 2 3 4 5))

; iseries filter
(check-equal? (ISeries-data (iseries-filter series-integer even?)) (vector 2 4))

(check-equal? (ISeries-data (iseries-filter series-integer odd?)) (vector 1 3))

(check-equal? (ISeries-data (iseries-filter-not series-integer even?)) (vector 1 3))

(check-equal? (ISeries-data (iseries-filter-not series-integer odd?)) (vector 2 4))

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
(iseries-print (assert (iseries-iloc series-integer (list 1 3)) ISeries?) (current-output-port))

(iseries-print series-integer (current-output-port))

(LabelIndex-index (build-multi-index-from-list (list (list 'a 'b 'c 'a 'c) (list 1 2 3 4 3))))

; iseries multi-index
(define multi-index-iseries (new-ISeries (vector 1 2 3 4 5) (build-multi-index-from-list (list (list 'a 'b 'c 'a 'c) (list 1 2 3 4 5)))))

(define multi-index-iseries-2 (new-ISeries (vector 500 2 3 4 100) (build-multi-index-from-list (list (list 'a 'b 'c 'a 'c) (list 1 2 3 4 3) (list 5 6 7 8 9)))))

(define multi-index-iseries-3 (new-ISeries (vector 100 200 300 400 500) (build-multi-index-from-list (list (list 'a 'b 'c 'a 'c) (list 1 2 3 4 3)))))

(check-equal? (iseries-loc-multi-index multi-index-iseries (list "a" "1")) 1)

(check-equal? (iseries-loc-multi-index multi-index-iseries-2 (list "c" "3" "9")) 100)

(iseries-print (assert (iseries-loc-multi-index multi-index-iseries-3 (list "c" "3")) ISeries?) (current-output-port))

(new-ISeries (vector 0 1 0 1 0 1 0 0 0 0 1 1 1 0 1) #f)