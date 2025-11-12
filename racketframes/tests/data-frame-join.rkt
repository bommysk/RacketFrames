;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*    Author: Shubham Kahal
;*    File: tests/data-frame-join.rkt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket
(require typed/rackunit)
(require racket/flonum)

(require "../data-frame/indexed-series.rkt")
(require "../data-frame/integer-series.rkt")
(require "../data-frame/integer-series-builder.rkt")
(require "../data-frame/numeric-series.rkt")
(require "../data-frame/numeric-series-builder.rkt")
(require "../data-frame/categorical-series.rkt")
(require "../data-frame/categorical-series-builder.rkt")
(require "../data-frame/generic-series.rkt")
(require "../data-frame/data-frame.rkt")
(require "../data-frame/data-frame-join.rkt")
(require "../data-frame/data-frame-print.rkt")
(require "../data-frame/series-print.rkt")

; ***********
; Test Cases
; ***********

(define integer-col-1 (cons 'integer-col-1 (new-ISeries (list 1 2 3 4 5))))

(define integer-col-2 (cons 'integer-col-2 (new-ISeries (list 6 7 8 9 10))))

(define float-col-1 (cons 'float-col-1 (new-NSeries (flvector 1.5 2.5 3.5 4.5 5.5))))

(define categorical-col-1 (cons' categorical-col-1 (new-CSeries (list 'a 'b 'c 'd 'e))))

; opaque
;(check-equal? (column-series integer-col-1)
;              (new-ISeries (list 1 2 3 4 5)))

(check-equal? (join-column-name integer-col-1 (set 'integer-col-1 'integer-col-2) "prefix")
              'prefixinteger-col-1)

; (: dest-mapping-series-builders (DataFrameDescription Index -> (Listof SeriesBuilder)))

(define columns-integer
  (list 
   (cons 'col1 (new-ISeries (list 1 2 3 4 2 )))
   (cons 'col2 (new-ISeries (list 5 6 7 8 6)))
   (cons 'col3 (new-ISeries (list 9 10 11 12 17)))
   (cons 'col4 (new-ISeries (list 13 14 15 16 18)))))

(define columns-categorical
  (list 
   (cons 'col1 (new-CSeries (list 'a 'b 'c 'd 'e)))
   (cons 'col2 (new-CSeries (list 'e 'f 'g 'h 'i)))
   (cons 'col3 (new-CSeries (list 'j 'k 'l 'm 'n)))))

; create new data-frame-integer
(define data-frame-integer (new-data-frame columns-integer))

; create new data-frame-categorical
(define data-frame-categorical (new-data-frame columns-categorical))

(data-frame-write-delim data-frame-integer)

(displayln "data-frame-groupby")
(data-frame-groupby data-frame-integer (list 'col1))

(data-frame-groupby data-frame-integer (list 'col2))

(data-frame-groupby data-frame-integer (list 'col1 'col2))

(displayln "data-frame-groupby aggregate mean")
(data-frame-write-delim (apply-agg-data-frame 'mean (data-frame-groupby data-frame-integer (list 'col1 'col2))))

(displayln "data-frame-groupby aggregate count")
(data-frame-write-delim (apply-agg-data-frame 'count (data-frame-groupby data-frame-integer (list 'col1 'col2))))


; unable to protect opaque value
;(check-equal?
 ;(dest-mapping-series-builders (data-frame-description data-frame-integer) 4)
 ;(list (new-ISeriesBuilder 4) (new-ISeriesBuilder 4) (new-ISeriesBuilder 4)))

; Unable to protect opaque
;(check-equal? (key-cols-sort-lexical (list integer-col-2 integer-col-1))
              ;(list (cons 'col2 (new-ISeries (list 5 6 7 8)))
                    ;(cons 'col3 (new-ISeries (list 9 10 11 12)))))

; Unable to protect opaque
;(check-equal? (key-cols-series (list integer-col-2 integer-col-1 float-col-1))
              ;(list (column-series integer-col-2) (column-series integer-col-1)))

(check-equal?
 ((key-fn (key-cols-series (list integer-col-2 integer-col-1 float-col-1 categorical-col-1))) 2)
 "8::3::3.5::c")

; build hash join
;(check-equal?
; (index (key-cols-series (list integer-col-2 integer-col-1 float-col-1 categorical-col-1)))
; (hash "6\t1\ta\t" (list 0)
;       "7\t2\tb\t" (list 1)
;       "8\t3\tc\t" (list 2)
;       "10\t5\t3\t" (list 4)
;       "9\t4\td\t" (list 3)))

(key-cols-series (list integer-col-2 integer-col-1 float-col-1 categorical-col-1))

(define cseries-1 (new-CSeries (list 'a 'b 'c 'd 'e)))

(define iseries-1 (new-ISeries (list 1 2 3 4 5)))

(define nseries-1 (new-NSeries (flvector 1.5 2.5 3.5 4.5 5.5)))

(define cseries-2 (new-CSeries (list 'a 'b 'c 'd 'l)))

(define iseries-2 (new-ISeries (list 1 2 3 4 5)))

(define nseries-2 (new-NSeries (flvector 1.5 2.5 3.5 4.5 5.5)))

(define cseries-builder-1 (new-CSeriesBuilder))

; internal function test
;(define cseries-copy-fn-1 (cseries-copy-fn cseries-1 cseries-builder-1))

;(cseries-copy-fn-1 1)

(define cseries-builder-1-complete (complete-CSeriesBuilder cseries-builder-1))

; expect list-ref expection
; (check-equal? (cseries-iref cseries-builder-1-complete (list 0)) (list 'b))

;(copy-column-row-error cseries-1 3)

(define cseries-builder-2 (new-CSeriesBuilder))

(define iseries-builder-1 (new-ISeriesBuilder))

(define nseries-builder-1 (new-NSeriesBuilder))

;(: copy-column-row ((Vectorof Series) (Vectorof SeriesBuilder) Index -> Void))

(copy-column-row (vector cseries-1 iseries-1 nseries-1)
                 (vector cseries-builder-2 iseries-builder-1 nseries-builder-1)
                 2)

(check-equal? (cseries-iref (complete-CSeriesBuilder cseries-builder-2) (list 0)) (list 'c))

(check-equal? (iseries-iref (complete-ISeriesBuilder iseries-builder-1) (list 0)) (list 3))

(check-equal? (nseries-iref (complete-NSeriesBuilder nseries-builder-1) (list 0)) (list 3.5))

; (: do-join-build-left/right ((Vectorof Series) (Vectorof Series)
;		  (Vectorof SeriesBuilder) (Vectorof SeriesBuilder)
;		  (Index -> Key) JoinHash -> Void))

(define fa-key-fn (key-fn (list cseries-1 iseries-1 cseries-2 iseries-2)))

(fa-key-fn 1)

(define cseries-builder-a (new-CSeriesBuilder))
(define iseries-builder-a (new-ISeriesBuilder))

(define cseries-builder-b (new-CSeriesBuilder))
(define iseries-builder-b (new-ISeriesBuilder))


;(series-data (complete-CSeriesBuilder cseries-builder-a))

;(series-data (complete-CSeriesBuilder cseries-builder-b))

;(data-frame-write-delim (data-frame-join-left data-frame-integer data-frame-categorical))

(define columns-integer-2
  (list 
   (cons 'col1 (new-ISeries (list 1 2 3 4)))
   (cons 'col2 (new-ISeries (list 5 6 7 8)))
   (cons 'col3 (new-ISeries (list 9 10 11 12)))
   (cons 'col4 (new-ISeries (list 21 22 23 24)))))

(define columns-integer-3
  (list 
   (cons 'col1 (new-ISeries (list 1 2 3 4)))
   (cons 'col2 (new-ISeries (list 25 26 27 28)))
   (cons 'col3 (new-ISeries (list 29 30 31 32)))
   (cons 'col4 (new-ISeries (list 101 201 301 401)))))

; create new data-frame-integer-2
(define data-frame-integer-2 (new-data-frame columns-integer-2))

; create new data-frame-integer-3
(define data-frame-integer-3 (new-data-frame columns-integer-3))

(data-frame-join-left data-frame-integer-2 data-frame-integer-3 #:on (list 'col1))

(data-frame-write-delim (data-frame-join-left data-frame-integer-2 data-frame-integer-3 #:on (list 'col1)))

(data-frame-write-delim (data-frame-join-left data-frame-integer-2 data-frame-integer-3 #:on (list 'col2)))

(data-frame-write-delim (data-frame-join-right data-frame-integer-2 data-frame-integer-3 #:on (list 'col1)))

(data-frame-write-delim (data-frame-join-right data-frame-integer-2 data-frame-integer-3 #:on (list 'col2)))

(define columns-integer-4
  (list 
   (cons 'col1 (new-ISeries (list 1 2 3 4)))
   (cons 'col2 (new-ISeries (list 5 6 7 8)))
   (cons 'col3 (new-ISeries (list 9 10 11 12)))
   (cons 'col4 (new-ISeries (list 21 22 23 24)))))

(define columns-integer-5
  (list 
   (cons 'col1 (new-ISeries (list 1 2 3 4)))
   (cons 'col2 (new-ISeries (list 5 6 7 8)))
   (cons 'col3 (new-ISeries (list 29 30 31 32)))
   (cons 'col4 (new-ISeries (list 101 201 301 401)))))

; create new data-frame-integer-4
(define data-frame-integer-4 (new-data-frame columns-integer-4))

; create new data-frame-integer-5
(define data-frame-integer-5 (new-data-frame columns-integer-5))

(data-frame-write-delim (data-frame-join-left data-frame-integer-4 data-frame-integer-5 #:on (list 'co3)))

(data-frame-write-delim (data-frame-join-inner data-frame-integer-2 data-frame-integer-3 #:on (list 'col1)))

(data-frame-write-delim (data-frame-join-inner data-frame-integer-4 data-frame-integer-5 #:on (list 'col2)))

(data-frame-write-delim (data-frame-join-outer data-frame-integer-4 data-frame-integer-5 #:on (list 'col2)))

(data-frame-write-delim (data-frame-join-outer data-frame-integer-4 data-frame-integer-5 #:on (list 'col3)))

(define columns-mixed-1
  (list 
   (cons 'col1 (new-ISeries (list 1 2 3 4)))
   (cons 'col2 (new-CSeries (list 'a 'b 'c 'd)))
   (cons 'col3 (new-ISeries (list 21 22 23 24)))))

(define columns-mixed-2
  (list 
   (cons 'col1 (new-ISeries (list 1 2 3 4)))
   (cons 'col2 (new-CSeries (list 'a 'b 'c 'd)))
   (cons 'col3 (new-ISeries (list 1 2 3 4)))))

; create new data-frame-mixed-1
(define data-frame-mixed-1 (new-data-frame columns-mixed-1))

; create new data-frame-mixed-2
(define data-frame-mixed-2 (new-data-frame columns-mixed-2))

;(data-frame-write-delim (data-frame-join-outer data-frame-mixed-1 data-frame-mixed-2 #:on (list 'col2)))

;(data-frame-write-delim (data-frame-join-outer data-frame-mixed-1 data-frame-mixed-2 #:on (list 'col3)))

(define columns-mixed-3
  (list 
   (cons 'col1 (new-ISeries (list 1 2 3 4)))
   (cons 'col2 (new-CSeries (list 'a 'b 'c 'd)))
   (cons 'col3 (new-ISeries (list 21 22 23 24)))))

(define columns-mixed-4
  (list 
   (cons 'col1 (new-ISeries (list 1 2 3 4)))
   (cons 'col2 (new-CSeries (list 'a 'b 'g 'd)))
   (cons 'col3 (new-ISeries (list 1 2 3 4)))))

; create new data-frame-mixed-3
(define data-frame-mixed-3 (new-data-frame columns-mixed-3))

; create new data-frame-mixed-4
(define data-frame-mixed-4 (new-data-frame columns-mixed-4))

;(data-frame-write-delim (data-frame-join-outer data-frame-mixed-3 data-frame-mixed-4 #:on (list 'col2)))

(define columns-mixed-5
  (list 
   (cons 'col1 (new-ISeries (list 1 2 3 4)))
   (cons 'col2 (new-CSeries (list 'a 'b 'c 'd)))
   (cons 'col3 (new-ISeries (list 21 22 23 24)))))

(define columns-mixed-6
  (list 
   (cons 'col1 (new-ISeries (list 11 21 31 41)))
   (cons 'col2 (new-CSeries (list 'a 'b 'g 'd)))
   (cons 'col3 (new-ISeries (list 22 22 23 24)))))

; create new data-frame-mixed-5
(define data-frame-mixed-5 (new-data-frame columns-mixed-5))

; create new data-frame-mixed-6
(define data-frame-mixed-6 (new-data-frame columns-mixed-6))

(data-frame-write-delim data-frame-mixed-5)

(data-frame-write-delim data-frame-mixed-6)

(data-frame-write-delim (data-frame-join-left data-frame-mixed-5 data-frame-mixed-6 #:on (list 'col3)))

(data-frame-write-delim (data-frame-join-inner data-frame-mixed-5 data-frame-mixed-6 #:on (list 'col2)))

(data-frame-write-delim (data-frame-join-right data-frame-mixed-5 data-frame-mixed-6 #:on (list 'col2)))

(data-frame-write-delim (data-frame-join-outer data-frame-mixed-5 data-frame-mixed-6 #:on (list 'col2)))

(define columns-mixed-7
  (list 
   (cons 'col1 (new-ISeries (list 1 2 3 4)))
   (cons 'col2 (new-CSeries (list 'a 'b 'c 'd)))
   (cons 'col3 (new-GenSeries (list 'a 1.5 20 10)))
   (cons 'col4 (new-ISeries (list 21 22 23 24)))))

(define columns-mixed-8
  (list 
   (cons 'col1 (new-ISeries (list 11 21 31 41)))
   (cons 'col2 (new-CSeries (list 'a 'b 'g 'd)))
   (cons 'col3 (new-GenSeries (list 'a 'b 'c 'd)))
   (cons 'col4 (new-ISeries (list 22 22 23 24)))))

; create new data-frame-mixed-7
(define data-frame-mixed-7 (new-data-frame columns-mixed-7))

; create new data-frame-mixed-8
(define data-frame-mixed-8 (new-data-frame columns-mixed-8))

(data-frame-write-delim data-frame-mixed-7)

(data-frame-write-delim data-frame-mixed-8)

(data-frame-write-delim (data-frame-join-left data-frame-mixed-7 data-frame-mixed-8 #:on (list 'col3)))

(data-frame-write-delim (data-frame-join-right data-frame-mixed-7 data-frame-mixed-8 #:on (list 'col3)))

(data-frame-write-delim (data-frame-join-inner data-frame-mixed-7 data-frame-mixed-8 #:on (list 'col3)))

(data-frame-write-delim (data-frame-join-outer data-frame-mixed-7 data-frame-mixed-8 #:on (list 'col2 'col3)))

(define columns-mixed-9
  (list 
   (cons 'col1 (new-ISeries (list 1 2 3 4)))
   (cons 'col2 (new-CSeries (list 'a 'b 'c 'd)))
   (cons 'col3 (new-GenSeries (list 'a 1.5 20 10)))
   (cons 'col4 (new-ISeries (list 21 22 23 24)))))

(define columns-mixed-10
  (list 
   (cons 'col1 (new-ISeries (list 1 2 3 4)))
   (cons 'col2 (new-CSeries (list 'a 'b 'c 'd)))
   (cons 'col3 (new-GenSeries (list 'a 1.5 20 10)))
   (cons 'col4 (new-ISeries (list 21 22 23 24)))))

; create new data-frame-mixed-9
(define data-frame-mixed-9 (new-data-frame columns-mixed-9))

; create new data-frame-mixed-10
(define data-frame-mixed-10 (new-data-frame columns-mixed-10))

(displayln "No provided join column test")
(data-frame-write-delim (data-frame-join-left data-frame-mixed-7 data-frame-mixed-8 #:on '()))

(data-frame-write-delim (data-frame-join-inner data-frame-mixed-9 data-frame-mixed-10 #:on '()))

(displayln "multi-index-from-cols test")
(build-multi-index-from-cols (list (new-ISeries (list 1 2 3 4))
                                   (new-CSeries (list 'a 'b 'c 'd))
                                   (new-GenSeries (list 'a 1.5 20 10))
                                   (new-ISeries (list 21 22 23 24))))

(build-multi-index-from-cols columns-mixed-10)

(define integer-series-groupby (new-ISeries (list 1 2 3 4 2 3 4 4 5 6 7 8 9)))
(iseries-groupby integer-series-groupby)

(define integer-series-groupby-label-index (new-ISeries (list 1 2 3 4 2 3 4 4 5 6 7 8 9) #:index (build-index-from-list (list 'a 'a 'a 'b 'c 'd 'e 'e 'f 'g 'e 'b 'c))))

(iseries-print integer-series-groupby-label-index)

(iseries-groupby integer-series-groupby-label-index)

(apply-agg-iseries 'sum (iseries-groupby integer-series-groupby-label-index))
