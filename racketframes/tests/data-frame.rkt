;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*    Author: Shubham Kahal
;*    File: tests/data-frame.rkt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket
(require typed/rackunit)

(require racket/fixnum)
(require racket/flonum)

(require "../data-frame/series-description.rkt"
         "../data-frame/series.rkt"
         "../data-frame/data-frame.rkt"
         "../data-frame/data-frame-print.rkt"
         (only-in "../data-frame/indexed-series.rkt"
    RFIndex label-sort-positional ListofLabel? Label? LabelIndex?
          ListofFlonum? ListofFixnum? ListofBoolean? ListofDatetime? ListofDate? Listofdate? ListofIndex?
          Label LabelProjection LabelProjection? LabelIndex LabelIndex-index
          build-index-from-labels build-index-from-list build-index-from-sequence
          label-index idx->key idx->label IndexDataType RFIndex? extract-index)
 (only-in "../data-frame/series-description.rkt"
    series-description series-length series-type 
          Series Series? SeriesType
          SeriesDescription SeriesDescription-name
          SeriesDescription-type SeriesDescription-length
          IndexableSeries IndexableSeries?)
 (only-in "../data-frame/series.rkt"
          series-data set-series-index get-series-index in-series
          series-loc-boolean series-loc series-iloc indexable-series->index)
 (only-in "../data-frame/generic-series.rkt"
         GenSeries GenericType GenSeries?
         GenSeries-data
         new-GenSeries)
 (only-in "../data-frame/categorical-series.rkt"
          CSeries CSeries?
          CSeries-data
          new-CSeries)
 (only-in "../data-frame/numeric-series.rkt"
          NSeries NSeries? 
          NSeries-data
          new-NSeries
          list->flvector)
 (only-in "../data-frame/integer-series.rkt"
    ISeries ISeries?
    ISeries-data
    new-ISeries
          list->fxvector iseries-print)
 (only-in "../data-frame/boolean-series.rkt"
    BSeries BSeries?
    BSeries-data
    new-BSeries)
 (only-in "../data-frame/datetime-series.rkt"
    DatetimeSeries DatetimeSeries?
    DatetimeSeries-data
    new-DatetimeSeries)
 (only-in "../data-frame/date-series.rkt"
    DateSeries DateSeries?
    DateSeries-data
    new-DateSeries derive-date-value))

; ***********************************************************
; Test Cases
; ***********************************************************
; data frame tests

;******************
;data-frame-integer
;******************
(define columns-integer
  (list 
   (cons 'col1 (new-ISeries (fxvector 1 2 3 4)
                            #:index (build-index-from-list (list 'a 'b 'c 'd))))
   (cons 'col2 (new-ISeries (fxvector 5 6 7 8)
                            #:index (build-index-from-list (list 'e 'f 'g 'h))))
   (cons 'col3 (new-ISeries (fxvector 9 10 11 12)
                            #:index (build-index-from-list (list 'i 'j 'k 'l))))))

; create new data-frame-integer
(define data-frame-integer (new-data-frame columns-integer))

(data-frame-iloc data-frame-integer (list 1 2) 2)

(data-frame-write-delim data-frame-integer)

;******************
;data-frame-float
;******************
(define columns-float
  (list 
   (cons 'col1 (new-NSeries (flvector 1.5 2.5 3.5 4.5)
                            #:index (build-index-from-list (list 'a 'b 'c 'd))))
   (cons 'col2 (new-NSeries (flvector 5.5 6.5 7.5 8.5)
                            #:index (build-index-from-list (list 'e 'f 'g 'h))))
   (cons 'col3 (new-NSeries (flvector 9.5 10.5 11.5 12.5)
                            #:index (build-index-from-list (list 'i 'j 'k 'l))))))

; create new data-frame-float
(define data-frame-float (new-data-frame columns-float))

(set! data-frame-float (data-frame-set-index data-frame-float (list 'a 'b 'c 'd)))

(hash->list (LabelIndex-index data-frame-float))

(label-index (LabelIndex-index data-frame-float) 'col1)

(data-frame-write-delim (assert (data-frame-iloc data-frame-float (list 1 2) (list 0 1)) DataFrame?))

(data-frame-write-delim data-frame-float)

;******************
;data-frame-categorical
;******************
; will define parse to automatically build this columns structure
(define columns-categorical
  (list 
   (cons 'col1 (new-CSeries (vector 'hello 'world)))
   (cons 'col2 (new-CSeries (vector 'fizz 'buzz)))
   (cons 'col3 (new-CSeries (vector 'foo 'bar)))))

; create new data-frame-categorical
(define data-frame-categorical (new-data-frame columns-categorical))

(data-frame-write-delim data-frame-categorical)

;******************
;data-frame-mix
;******************
(define columns-mix
  (list
   (cons 'integer-col (new-ISeries (fxvector 1 2 3 4)
                            #:index (build-index-from-list (list 'a 'b 'c 'd))))
   (cons 'float-col (new-NSeries (flvector 1.5 2.5 3.5 4.5)
                            #:index (build-index-from-list (list 'a 'b 'c 'd))))
   (cons 'categorical-col (new-CSeries (vector 'hello 'world 'fizz 'buzz)))))

; create new data-frame-mix
(define data-frame-mix (new-data-frame columns-mix))

(data-frame-write-delim data-frame-mix)

; ************************
; data-frame-integer tests
; ************************

(check-equal? (series-data (data-frame-series-ref data-frame-integer 'col1))
              (vector 1 2 3 4))

(check-equal? (series-data (data-frame-series-ref data-frame-integer 'col2))
              (vector 5 6 7 8))

(set! data-frame-integer (data-frame-rename data-frame-integer 'col1 'col-one))

(check-equal? (data-frame-names data-frame-integer) (list 'col-one 'col2 'col3))

(check-equal? (series-data (data-frame-series-ref data-frame-integer 'col-one))
              (vector 1 2 3 4))

(check-equal? (data-frame-labels data-frame-integer)
              (list '(col2 1) '(col3 2) '(col-one 0)))

(check-equal? (projection-set (list 'col-one 'col2 'col3))
              (set 'col-one 'col2 'col3))

(check-equal? (data-frame-all-labels-projection-set data-frame-integer)
              (set 'col-one 'col2 'col3))

; check error
;(data-frame-series data-frame-integer 'col1)

(set! data-frame-integer (data-frame-drop data-frame-integer 'col-one))

; check col-one is gone

; ************************
; data-frame-float tests
; ************************

(check-equal? (series-data (data-frame-series-ref data-frame-float 'col1))
              (flvector 1.5 2.5 3.5 4.5))

(check-equal? (series-data (data-frame-series-ref data-frame-float 'col2))
              (flvector 5.5 6.5 7.5 8.5))

(set! data-frame-float (data-frame-rename data-frame-float 'col1 'col-one))

(check-equal? (data-frame-names data-frame-float) (list 'col-one 'col2 'col3))

(check-equal? (series-data (data-frame-series-ref data-frame-float 'col-one))
              (flvector 1.5 2.5 3.5 4.5))

; check error
;(data-frame-series data-frame-float 'col1)

(set! data-frame-float (data-frame-drop data-frame-float 'col-one))

; check col-one is gone

(data-frame-write-delim data-frame-float)

; ************************
; data-frame-categorical tests
; ************************

(check-equal? (series-data (data-frame-series-ref data-frame-categorical 'col1))
              (vector 'hello 'world))

(check-equal? (series-data (data-frame-series-ref data-frame-categorical 'col2))
              (vector 'fizz 'buzz))

(set! data-frame-categorical (data-frame-rename data-frame-categorical 'col1 'col-one))

(check-equal? (data-frame-names data-frame-categorical) (list 'col-one 'col2 'col3))

(check-equal? (series-data (data-frame-series-ref data-frame-categorical 'col-one))
              (vector 'hello 'world))

; check error
;(data-frame-series data-frame-float 'col1)

(set! data-frame-categorical (data-frame-drop data-frame-categorical 'col-one))

; check col-one is gone
(check-equal? (data-frame-names data-frame-categorical) (list 'col2 'col3))

; ************************
; data-frame-mix tests
; ************************

(check-equal? (series-data (data-frame-series-ref data-frame-mix 'integer-col))
              (vector 1 2 3 4))

(check-equal? (series-data (data-frame-series-ref data-frame-mix 'float-col))
              (flvector 1.5 2.5 3.5 4.5))

(check-equal? (series-data (data-frame-series-ref data-frame-mix 'categorical-col))
              (vector 'hello 'world 'fizz 'buzz))

(set! data-frame-mix (data-frame-rename data-frame-mix 'float-col 'float-column))

(check-equal? (data-frame-names data-frame-mix) (list 'integer-col 'float-column 'categorical-col))

(check-equal? (series-data (data-frame-series-ref data-frame-mix 'float-column))
              (flvector 1.5 2.5 3.5 4.5))

; check error
;(data-frame-series data-frame-float 'col1)

(set! data-frame-mix (data-frame-drop data-frame-mix 'float-column))

; check float-column is gone

(check-equal? (data-frame-names data-frame-mix) (list 'integer-col 'categorical-col))

; data-frame-explode tests
(data-frame-explode data-frame-integer)

(data-frame-description data-frame-integer)

(show-data-frame-description (data-frame-description data-frame-integer))

(check-equal? (series-iloc (data-frame-series-ref data-frame-integer 'col3) 2) 11)

(set! data-frame-integer (data-frame-set-index data-frame-integer (list 'a 'b 'c 'd)))
;(LabelIndex-index (cdr (list-ref (data-frame-explode data-frame-integer) 0)))

(data-frame-write-delim data-frame-integer)

(check-equal? (series-data (data-frame-series-ref data-frame-integer 'col3)) (vector 9 10 11 12))

(check-equal? (series-loc (data-frame-series-ref data-frame-integer 'col3) 'c) 11)

(set! data-frame-integer (data-frame-set-index data-frame-integer (list 'ind1 'ind2 'ind3 'ind4)))

(check-equal? (series-loc (data-frame-series-ref data-frame-integer 'col2) 'ind2) 6)

(data-frame-series-ref data-frame-integer 'col2)

(iseries-print (assert (data-frame-series-ref data-frame-integer 'col2) ISeries?))

(iseries-print (assert (data-frame-series-ref data-frame-integer 'col3) ISeries?))

(check-equal? (data-frame-contains? data-frame-integer (list 'col2 'col3)) #t)

(check-equal? (data-frame-contains? data-frame-integer (list 'col1 'col4 'col6)) #f)

(check-equal? (data-frame-contains/any? data-frame-integer (list 'col2 'col4 'col6)) #t)

(check-equal? (data-frame-row-count data-frame-integer) 4)

(check-equal? (data-frame-column-count data-frame-integer) 2)

(define data-frame-from-hash (new-data-frame (hash 'a (list 1 2 3 7 8) 'b (list 3 5 6 10 10) 'c (list 3.4 5.5 6.7 4.0 95.6) 'd (list 'fizz 'buzz 'baz 'fizz 'fizz))))

(data-frame-head data-frame-from-hash)

