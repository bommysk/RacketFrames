;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*    Author: Shubham Kahal
;*    File: tests/plot.rkt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket

(require typed/rackunit)
(require plot)
(require plot/utils)

(require
  racket/pretty
  racket/unsafe/ops
  racket/flonum
  racket/fixnum
  racket/set
  racket/vector
   (only-in "../data-frame/series-description.rkt"
	  SeriesType Series Series? SeriesList SeriesList?
	  SeriesDescription-type series-type series-length)
   (only-in "../data-frame/series.rkt"
	  series-iref  
          series-data get-series-index has-series-index?)
   (only-in "../data-frame/data-frame.rkt"
	  DataFrame DataFrame? Column Columns Columns? Column? new-data-frame data-frame-names
	  data-frame-cseries data-frame-explode column-series
	  DataFrameDescription DataFrameDescription-series data-frame-description)
  (only-in "../data-frame/generic-series.rkt"
           GenSeries GenSeries? GenericType gen-series-iref new-GenSeries
           gen-series-referencer)
  (only-in "../data-frame/numeric-series.rkt"
           NSeries NSeries? nseries-iref nseries-index-ref new-NSeries)
  (only-in "../data-frame/integer-series.rkt"
           ISeries ISeries? iseries-iref new-ISeries
           iseries-referencer)
  (only-in "../data-frame/categorical-series.rkt"
           cseries-referencer cseries-length cseries-iref
           CSeries CSeries? new-CSeries)
  (only-in "../data-frame/indexed-series.rkt"
           build-index-from-list))

(require "../plot/plot.rkt")

;******************
; Test Cases
;******************

(displayln "\n")
(displayln "plotting integer series")

(make-scatter-plot (new-ISeries (fxvector 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)))

(make-line-plot (new-ISeries (fxvector 1 2 3 4 5)))

(define float-column (cons 'col1 (new-NSeries (flvector 1.5 2.5 3.5 4.5))))

(displayln "plotting float columns")

(make-scatter-plot float-column)

(make-line-plot float-column)

;******************
;data-frame-integer
;******************
(define integer-columns
  (list 
   (cons 'col1 (new-ISeries (fxvector 1 2 3 4 4)))
   (cons 'col2 (new-ISeries (fxvector 5 6 7 8 24)))
   (cons 'col3 (new-ISeries (fxvector 9 10 11 12 24)))))

; create new data-frame-integer
(define data-frame-integer (new-data-frame integer-columns))

(displayln "plotting integer data-frame")

(make-scatter-plot data-frame-integer)

(displayln "plotting integer columns")

(make-scatter-plot integer-columns)

;******************
;data-frame-float
;******************
(define float-columns
  (list 
   (cons 'col1 (new-NSeries (flvector 1.5 2.5 3.5 4.5)))
   (cons 'col2 (new-NSeries (flvector 5.5 6.5 7.5 8.5)))
   (cons 'col3 (new-NSeries (flvector 9.5 10.5 11.5 12.5)))))

; create new data-frame-float
(define data-frame-float (new-data-frame float-columns))

(displayln "plotting float data-frame")

(make-scatter-plot data-frame-float)

(displayln "plotting float columns")

(make-scatter-plot float-columns)

(displayln "discrete histogram")

(make-discrete-histogram (new-ISeries (list 1 2 2 3 4 5 5 5)))

(make-discrete-histogram (new-GenSeries (list 1 2 2 1.5 3 4 5 5 'a 5)))

(make-discrete-histogram float-column)

(make-discrete-histogram integer-columns)

(make-discrete-histogram data-frame-float)

(get-index-vector (new-ISeries (fxvector 1 2 3 4 4)))

(define series-integer-labeled
  (new-ISeries (fxvector 1 2 3 4)
               #:index (list 'a 'b 'c 'd)))

(get-index-vector series-integer-labeled)

(make-discrete-histogram-stacked float-column)

(make-discrete-histogram-stacked float-columns)

;******************
;data-frame-integer
;******************
(define integer-columns-2
  (list 
   (cons 'col1 (new-ISeries (fxvector 1 2 3 4)
                            #:index (build-index-from-list (list 'a 'b 'c 'd))))
   (cons 'col2 (new-ISeries (fxvector 5 6 7 8)
                            #:index (build-index-from-list (list 'e 'f 'g 'h))))
   (cons 'col3 (new-ISeries (fxvector 9 10 11 12)
                            #:index (build-index-from-list (list 'i 'j 'k 'l))))))

; create new data-frame-integer
(define data-frame-integer-2 (new-data-frame integer-columns-2))

(make-discrete-histogram-stacked data-frame-integer-2)

; individual testing does not work due to Racket type checking
;(get-discrete-hist-data (vector 1 2 3 4 5))

;(list-of-vec-from-hist-bin (get-discrete-hist-data (vector 1 2 3 4 5)))


;******************

(displayln "plotting float data-frame")

(make-scatter-plot data-frame-float)