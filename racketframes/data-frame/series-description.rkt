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
; Provide functions in this file to other files.

(provide
 (struct-out SeriesDescription)
 Series Series? SeriesList SeriesList? SeriesType)

(provide: 
 [series-description (Label Series -> SeriesDescription)]
 [series-type (Series -> SeriesType)]
 [series-length (Series -> Index)]
 [series-data (Series -> (U (Vectorof GenericType) FlVector (Vectorof Symbol) (Vectorof Fixnum) (Vectorof Boolean) (Vectorof Datetime)))]
 [series-iref (Series Index -> Any)]
 [series-index-ref (Series IndexDataType -> Any)]
 [series-loc-boolean (Series (Listof Boolean) -> (U Any Series))]
 [series-loc (Series (U Label (Listof Label) (Listof Boolean)) -> (U Any Series))]
 [series-iloc (Series (U Index (Listof Index)) -> (U Any Series))]
 [set-series-index (Series RFIndex -> Series)]
 [get-series-index (Series -> RFIndex)]
 [has-series-index? (Series -> Boolean)])

; ***********************************************************

; ***********************************************************

(require
  racket/sequence
 (only-in racket/flonum
          flvector-length)
 (only-in "indexed-series.rkt"
          Label RFIndex IndexDataType ListofListofString ListofListofString?)
 (only-in "generic-series.rkt"
          GenericType GenSeries GenSeries? new-GenSeries GenSeries-index GenSeries-data gen-series-length gen-series-data gen-series-index gen-series-iref
          set-GenSeries-index gen-series-loc-boolean gen-series-loc gen-series-loc-multi-index gen-series-iloc gen-series-index-ref)
 (only-in "categorical-series.rkt"
          CSeries CSeries? new-CSeries CSeries-index CSeries-data cseries-length cseries-data cseries-index cseries-iref set-CSeries-index
          cseries-loc-boolean cseries-iloc cseries-loc cseries-loc-multi-index cseries-index-ref)
 (only-in "numeric-series.rkt"
          NSeries NSeries? new-NSeries NSeries-index NSeries-data nseries-length nseries-data nseries-index nseries-iref
          set-NSeries-index nseries-loc-boolean nseries-loc nseries-loc-multi-index nseries-iloc nseries-index-ref list->flvector)
 (only-in "integer-series.rkt"
	  ISeries ISeries? new-ISeries ISeries-index ISeries-data iseries-length iseries-data iseries-index iseries-iref
          set-ISeries-index iseries-loc-boolean iseries-loc iseries-loc-multi-index iseries-iloc iseries-index-ref)
 (only-in "boolean-series.rkt"
	  BSeries BSeries? new-BSeries BSeries-index BSeries-data bseries-length bseries-data bseries-index bseries-iref
          set-BSeries-index bseries-loc-boolean bseries-loc bseries-loc-multi-index bseries-iloc bseries-index-ref)
 (only-in "datetime-series.rkt"
	  DatetimeSeries DatetimeSeries? new-DatetimeSeries DatetimeSeries-index DatetimeSeries-data datetime-series-length datetime-series-data datetime-series-index datetime-series-iref
          set-DatetimeSeries-index datetime-series-loc-boolean datetime-series-loc datetime-series-loc-multi-index datetime-series-iloc datetime-series-index-ref)
 (only-in "../util/datetime/types.rkt"
          Datetime))

; ***********************************************************

; ***********************************************************

(define-type Series (U GenSeries NSeries CSeries ISeries BSeries DatetimeSeries))

(define-predicate Series? Series)

(define-type SeriesList (Listof Series))

(define-predicate SeriesList? SeriesList)

(define-type SeriesType (U 'GenericSeries 'NumericSeries 'CategoricalSeries 'IntegerSeries 'BooleanSeries 'DatetimeSeries))

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
    [else (error "Unknown Series type in DataFrame")]))

(: series-description  (Label Series -> SeriesDescription))
(define (series-description name series)
  (SeriesDescription name (series-type series) (series-length series)))

; ***********************************************************

; ***********************************************************
; Get series data

(: series-data (Series -> (U (Vectorof GenericType) FlVector (Vectorof Symbol)
                             (Vectorof Fixnum) (Vectorof Boolean) (Vectorof Datetime))))
(define (series-data series)
  (cond
    [(GenSeries? series) (gen-series-data series)]
    [(NSeries? series) (nseries-data series)]    
    [(CSeries? series) (cseries-data series)]    
    [(ISeries? series) (iseries-data series)]
    [(BSeries? series) (bseries-data series)]
    [(DatetimeSeries? series) (datetime-series-data series)]
    [else (error "Unknown Series type in DataFrame")]))

(: series-iref (Series Index -> Any))
(define (series-iref series idx)
  (cond
    [(GenSeries? series) (gen-series-iref series (list idx))]
    [(NSeries? series) (nseries-iref series (list idx))]    
    [(CSeries? series) (cseries-iref series (list idx))]    
    [(ISeries? series) (iseries-iref series (list idx))]
    [(BSeries? series) (bseries-iref series (list idx))]
    [(DatetimeSeries? series) (datetime-series-iref series (list idx))]
    [else (error "Unknown Series type in DataFrame")]))

(: series-index-ref (Series IndexDataType -> Any))
(define (series-index-ref series idx)
  (cond
    [(GenSeries? series) (gen-series-index-ref series idx)]
    [(NSeries? series) (nseries-index-ref series idx)]    
    [(CSeries? series) (cseries-index-ref series idx)]    
    [(ISeries? series) (iseries-index-ref series idx)]
    [(BSeries? series) (bseries-index-ref series idx)]
    [(DatetimeSeries? series) (datetime-series-index-ref series idx)]
    [else (error "Unknown Series type in DataFrame")]))

(: series-loc-boolean (Series (Listof Boolean) -> (U Any Series)))
(define (series-loc-boolean series boolean-lst)
  (cond
    [(GenSeries? series) (gen-series-loc-boolean series boolean-lst)]
    [(NSeries? series) (nseries-loc-boolean series boolean-lst)]
    [(CSeries? series) (cseries-loc-boolean series boolean-lst)]   
    [(ISeries? series) (iseries-loc-boolean series boolean-lst)]
    [(BSeries? series) (bseries-loc-boolean series boolean-lst)]
    [(DatetimeSeries? series) (datetime-series-loc-boolean series boolean-lst)]
    [else (error "Unknown Series type in DataFrame")]))

(: series-loc (Series (U Label (Listof Label) (Listof Boolean)) -> (U Any Series)))
(define (series-loc series label)
  (cond
    [(GenSeries? series) (gen-series-loc series label)]
    [(NSeries? series) (nseries-loc series label)]
    [(CSeries? series) (cseries-loc series label)]   
    [(ISeries? series) (iseries-loc series label)]
    [(BSeries? series) (bseries-loc series label)]
    [(DatetimeSeries? series) (datetime-series-loc series label)]
    [else (error "Unknown Series type in DataFrame")]))

(: series-loc-multi-index (Series (U (Listof String) ListofListofString) -> (U Any Series)))
(define (series-loc-multi-index series label)
  (cond
    [(GenSeries? series) (gen-series-loc-multi-index series label)]
    [(NSeries? series) (nseries-loc-multi-index series label)]
    [(CSeries? series) (cseries-loc-multi-index series label)]   
    [(ISeries? series) (iseries-loc-multi-index series label)]
    [(BSeries? series) (bseries-loc-multi-index series label)]
    [(DatetimeSeries? series) (datetime-series-loc-multi-index series label)]
    [else (error "Unknown Series type in DataFrame")]))

(: series-iloc (Series (U Index (Listof Index)) -> (U Any Series)))
(define (series-iloc series idx)
  (cond
    [(GenSeries? series) (gen-series-iloc series idx)]
    [(NSeries? series) (nseries-iloc series idx)]
    [(CSeries? series) (cseries-iloc series idx)]   
    [(ISeries? series) (iseries-iloc series idx)]
    [(BSeries? series) (bseries-iloc series idx)]
    [(DatetimeSeries? series) (datetime-series-iloc series idx)]
    [else (error "Unknown Series type in DataFrame")]))

; ***********************************************************

; ***********************************************************

(: set-series-index (Series RFIndex -> Series))
(define (set-series-index series index)
  (cond
    [(GenSeries? series) (set-GenSeries-index (assert series GenSeries?) index)]
    [(NSeries? series) (set-NSeries-index (assert series NSeries?) index)]
    [(CSeries? series) (set-CSeries-index (assert series CSeries?) index)]    
    [(ISeries? series) (set-ISeries-index (assert series ISeries?) index)]
    [(BSeries? series) (set-BSeries-index (assert series BSeries?) index)]
    [(DatetimeSeries? series) (set-DatetimeSeries-index (assert series DatetimeSeries?) index)]
    [else (error "Unknown or not supported series type in DataFrame")]))

(: get-series-index (Series -> RFIndex))
(define (get-series-index series)
  (cond
    [(GenSeries? series) (assert (gen-series-index (assert series GenSeries?)))]
    [(NSeries? series) (assert (nseries-index (assert series NSeries?)))]
    [(CSeries? series) (assert (cseries-index (assert series CSeries?)))]    
    [(ISeries? series) (assert (iseries-index (assert series ISeries?)))]
    [(BSeries? series) (assert (bseries-index (assert series BSeries?)))]
    [(DatetimeSeries? series) (assert (datetime-series-index (assert series DatetimeSeries?)))]
    [else (error "Unknown or not supported series type in DataFrame")]))

(: has-series-index? (Series -> Boolean))
(define (has-series-index? series)
  (cond
    [(GenSeries? series) (if (GenSeries-index (assert series GenSeries?)) #t #f)]
    [(NSeries? series) (if (NSeries-index (assert series NSeries?)) #t #f)]
    [(CSeries? series) (if (CSeries-index (assert series CSeries?)) #t #f)]
    [(ISeries? series) (if (ISeries-index (assert series ISeries?)) #t #f)]
    [(BSeries? series) (if (BSeries-index (assert series BSeries?)) #t #f)]
    [(DatetimeSeries? series) (if (DatetimeSeries-index (assert series DatetimeSeries?)) #t #f)]
    [else (error "Unknown or not supported series type in DataFrame")]))

; ***********************************************************
