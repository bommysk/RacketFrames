;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*    Author: Shubham Kahal
;*    File: series-description.rkt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket

; ***********************************************************
; A map of series to label names, represented as a collection
; of columns.
; ***********************************************************

; ***********************************************************

(require
  racket/sequence
  typed/racket/date
 (only-in racket/flonum
          flvector-length flvector?)
 (only-in "indexed-series.rkt"
          Label RFIndex IndexDataType ListofListofString ListofListofString? ListofIndexDataType? build-index-from-sequence)
 (only-in "generic-series.rkt"
          GenericType GenSeries GenSeries? new-GenSeries GenSeries-index GenSeries-data gen-series-length gen-series-data gen-series-index gen-series-iref
          set-GenSeries-index gen-series-loc-boolean gen-series-loc gen-series-loc-multi-index gen-series-iloc gen-series-index-ref)
 (only-in "categorical-series.rkt"
          CSeries CSeries? new-CSeries CSeries-index CSeries-data cseries-length cseries-data cseries-index cseries-iref set-CSeries-index
          cseries-loc-boolean cseries-iloc cseries-loc cseries-loc-multi-index cseries-index-ref)
 (only-in "numeric-series.rkt"
          NSeries NSeries? new-NSeries NSeries-index NSeries-data nseries-length nseries-data nseries-index nseries-iref
          set-NSeries-index nseries-loc-boolean nseries-loc nseries-loc-multi-index nseries-iloc nseries-index-ref list->flvector flvector->list)
 (only-in "integer-series.rkt"
	  ISeries ISeries? new-ISeries ISeries-index ISeries-data iseries-length iseries-data iseries-index iseries-iref
          set-ISeries-index iseries-loc-boolean iseries-loc iseries-loc-multi-index iseries-iloc iseries-index-ref RFFixnum RFFixnum?)
 (only-in "boolean-series.rkt"
	  BSeries BSeries? new-BSeries BSeries-index BSeries-data bseries-length bseries-data bseries-index bseries-iref
          set-BSeries-index bseries-loc-boolean bseries-loc bseries-loc-multi-index bseries-iloc bseries-index-ref)
 (only-in "datetime-series.rkt"
	  DatetimeSeries DatetimeSeries? new-DatetimeSeries DatetimeSeries-index DatetimeSeries-data datetime-series-length datetime-series-data datetime-series-index datetime-series-iref
          set-DatetimeSeries-index datetime-series-loc-boolean datetime-series-loc datetime-series-loc-multi-index datetime-series-iloc datetime-series-index-ref)
 (only-in "date-series.rkt"
	  DateSeries DateSeries? new-DateSeries DateSeries-index DateSeries-data date-series-length date-series-data date-series-index date-series-iref
          set-DateSeries-index date-series-loc-boolean date-series-loc date-series-loc-multi-index date-series-iloc date-series-index-ref)
 (only-in "../util/datetime/types.rkt"
          Datetime))

; ***********************************************************

; ***********************************************************
; Provide functions in this file to other files.

(provide
 (struct-out SeriesDescription)
 Series Series? SeriesList SeriesList? SeriesType IndexableSeries IndexableSeries?)

(provide: 
 [series-description (Label Series -> SeriesDescription)]
 [series-type (Series -> SeriesType)]
 [series-length (Series -> Index)])
; ***********************************************************

; ***********************************************************

(define-type Series (U GenSeries NSeries CSeries ISeries BSeries DatetimeSeries DateSeries))

(define-predicate Series? Series)

(define-type SeriesList (Listof Series))

(define-predicate SeriesList? SeriesList)

(define-type SeriesType (U 'GenericSeries 'NumericSeries 'CategoricalSeries 'IntegerSeries 'BooleanSeries 'DatetimeSeries 'DateSeries))

(define-type IndexableSeries (U GenSeries CSeries ISeries NSeries DateSeries))

(define-predicate IndexableSeries? IndexableSeries)

(struct: SeriesDescription ([name : Label]
                            [type : SeriesType]
                            [length : Integer]) #:transparent)

; ***********************************************************

; ***********************************************************
(: series-type (Series -> SeriesType))
(define (series-type series)
  (cond
   ((GenSeries? series) 'GenericSeries)
   ((NSeries? series) 'NumericSeries)
   ((CSeries? series) 'CategoricalSeries)
   ((ISeries? series) 'IntegerSeries)
   ((BSeries? series) 'BooleanSeries)
   ((DatetimeSeries? series) 'DatetimeSeries)
   ((DateSeries? series) 'DateSeries)
   (else (error "Unknown Series type in DataFrame"))))

(: series-length (Series -> Index))
(define (series-length series)
  (cond
    [(GenSeries? series) (gen-series-length series)]
    [(NSeries? series) (nseries-length series)]
    [(CSeries? series) (cseries-length series)]    
    [(ISeries? series) (iseries-length series)]
    [(BSeries? series) (bseries-length series)]
    [(DatetimeSeries? series) (datetime-series-length series)]
    [(DateSeries? series) (date-series-length series)]
    [else (error "Unknown Series type in DataFrame")]))

(: series-description  (Label Series -> SeriesDescription))
(define (series-description name series)
  (SeriesDescription name (series-type series) (series-length series)))

; ***********************************************************