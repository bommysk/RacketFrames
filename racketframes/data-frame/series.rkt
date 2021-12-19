#lang typed/racket

(provide SeriesDataVectorType)

(provide:
 [new-series ((Sequenceof Any) [#:index (Option (U (Listof IndexDataType) RFIndex))] -> Series)]
 [set-series-index (Series (U (Listof IndexDataType) RFIndex) -> Series)]
 [series-set-null-value (Series GenericType -> Series)]
 [series-groupby (Series [#:by-value Boolean] -> GroupHash)]
 [series-complete (SeriesBuilder -> Series)]
 [series-data (Series -> (U (Vectorof GenericType) FlVector (Vectorof Symbol) (Vectorof RFFixnum) (Vectorof Boolean) (Vectorof RFDatetime) (Vectorof RFDate)))]
 [series-iref (Series Index -> Any)]
 [series-index-ref (Series IndexDataType -> Any)]
 [series-referencer (Series -> (-> Index Any))]
 [series-loc-boolean (Series (Listof Boolean) -> (U Any Series))]
 [series-loc (Series (U Label (Listof Label) (Listof Boolean)) -> (U Any Series))]
 [series-iloc (Series (U Index (Listof Index)) -> (U Any Series))]
 [get-series-index (Series -> RFIndex)]
 [has-series-index? (Series -> Boolean)]
 [in-series (GenericType Series -> Boolean)]
 [indexable-series->index (IndexableSeries -> RFIndex)]
 [series-data->indexable-sequence ((U (Vectorof Any) (Vectorof Boolean) (Vectorof RFDatetime) (Vectorof RFFixnum) (Vectorof Symbol) (Vectorof RFDate) FlVector) -> (Sequenceof IndexDataType))])


(require
  racket/fixnum
  racket/flonum
  racket/vector
  (only-in "indexed-series.rkt"
          Label RFIndex IndexDataType ListofFlonum? ListofFixnum? ListofBoolean? ListofDatetime? ListofDate? Listofdate? ListofLabel? ListofIndexDataType? build-index-from-sequence ListofListofString)
  (only-in "series-description.rkt"
           Series Series? SeriesList SeriesList? SeriesType IndexableSeries IndexableSeries? series-type)
  (only-in "groupby-util.rkt" gen-series-groupby [GroupHash gen-series-grouphash])
  (only-in "generic-series.rkt"
           GenericType GenSeries GenSeries? new-GenSeries gen-series-length gen-series-data gen-series-index gen-series-iref in-gen-series
           set-GenSeries-index gen-series-loc-boolean gen-series-loc gen-series-loc-multi-index gen-series-iloc gen-series-index-ref
           set-GenSeries-null-value set-GenSeries-any-null-value-inplace gen-series-null-value gen-series-referencer map/gen-s gen-series-filter)
  (only-in "categorical-series.rkt"
           CSeries CSeries? new-CSeries cseries-length cseries-data cseries-index cseries-iref in-cseries set-CSeries-index
           cseries-referencer cseries-loc-boolean cseries-iloc cseries-loc cseries-loc-multi-index cseries-index-ref
           set-CSeries-null-value cseries-null-value cseries-groupby cseries-grouphash cseries-filter cseries-filter-not)
  (only-in "series-iter.rkt" cseries-map)
  (only-in "numeric-series.rkt"
           NSeries NSeries? new-NSeries nseries-length nseries-data nseries-index nseries-iref nseries-referencer in-nseries
           set-NSeries-index nseries-loc-boolean nseries-loc nseries-loc-multi-index nseries-iloc nseries-index-ref list->flvector flvector->list
           set-NSeries-null-value nseries-null-value nseries-groupby nseries-grouphash flvector->vector map/ns nseries-filter)
  (only-in "integer-series.rkt"
           ISeries ISeries? new-ISeries iseries-length iseries-data iseries-index in-iseries iseries-iref iseries-referencer
           set-ISeries-index iseries-loc-boolean iseries-loc iseries-loc-multi-index iseries-iloc iseries-index-ref RFFixnum RFFixnum?
           set-ISeries-null-value iseries-null-value iseries-groupby iseries-grouphash map/is iseries-filter)
  (only-in "boolean-series.rkt"
           BSeries BSeries? new-BSeries bseries-length bseries-data bseries-index bseries-iref in-bseries bseries-referencer
           set-BSeries-index bseries-loc-boolean bseries-loc bseries-loc-multi-index bseries-iloc bseries-index-ref
           set-BSeries-null-value bseries-null-value bseries-groupby bseries-grouphash map/bs)
  (only-in "datetime-series.rkt"
          DatetimeSeries DatetimeSeries? new-DatetimeSeries DatetimeSeries-index DatetimeSeries-data datetime-series-length datetime-series-data datetime-series-index
          datetime-series-iref datetime-series-referencer in-datetime-series
          set-DatetimeSeries-index datetime-series-loc-boolean datetime-series-loc datetime-series-loc-multi-index datetime-series-iloc datetime-series-index-ref datetime-series-groupby
          set-DatetimeSeries-null-value datetime-series-null-value datetime-series-grouphash RFDatetime RFDatetime? map/datetime-series-data)
  (only-in "date-series.rkt"
           DateSeries DateSeries? new-DateSeries DateSeries-index DateSeries-data date-series-length date-series-data date-series-index date-series-iref date-series-referencer
           in-date-series date-series-groupby date-series-grouphash
           set-DateSeries-index set-DateSeries-null-value date-series-null-value date-series-loc-boolean date-series-loc date-series-loc-multi-index date-series-iloc date-series-index-ref
           RFDate RFDate? map/date-series-data)
  
  (only-in "series-builder.rkt" SeriesBuilder)
  (only-in "generic-series-builder.rkt"
           GenSeriesBuilder GenSeriesBuilder?
           complete-GenSeriesBuilder)
  (only-in "categorical-series-builder.rkt"
           CSeriesBuilder CSeriesBuilder?
           complete-CSeriesBuilder)
  (only-in "numeric-series-builder.rkt"
           NSeriesBuilder NSeriesBuilder?
           complete-NSeriesBuilder)
  (only-in "integer-series-builder.rkt"
           ISeriesBuilder ISeriesBuilder?
           complete-ISeriesBuilder)
  (only-in "boolean-series-builder.rkt"
           BSeriesBuilder BSeriesBuilder?
           complete-BSeriesBuilder)
  (only-in "datetime-series-builder.rkt"
           DatetimeSeriesBuilder DatetimeSeriesBuilder?
           complete-DatetimeSeriesBuilder)
  (only-in "date-series-builder.rkt"
           DateSeriesBuilder DateSeriesBuilder?
           complete-DateSeriesBuilder)
  (only-in "../load/sample.rkt"
           guess-series-type)
  (only-in "../util/datetime/types.rkt"
           Datetime Datetime?))

; add option to disable sampling and make GenericSeries by default
(: new-series ((Sequenceof Any) [#:index (Option (U (Listof IndexDataType) RFIndex))] -> Series))
(define (new-series data #:index [labels #f])
  (let*: ((series-type : SeriesType (guess-series-type (map ~a (sequence->list data)))))
    (cond                                                      
      [(eq? series-type 'CategoricalSeries)
       (new-CSeries (assert data ListofLabel?) #:index labels)]
      [(eq? series-type 'NumericSeries)
       (new-NSeries (list->flvector (assert (map (lambda ([num : GenericType]) (exact->inexact (assert num number?))) (sequence->list data)) ListofFlonum?)) #:index labels)]
      [(eq? series-type 'IntegerSeries)
       (new-ISeries (assert (sequence->list data) ListofFixnum?) #:index labels)]
      [(eq? series-type 'BooleanSeries)
       (new-BSeries (list->vector (assert (sequence->list data) ListofBoolean?)) #:index labels)]
      [(eq? series-type 'DatetimeSeries)
       (new-DatetimeSeries (list->vector (assert (sequence->list data) ListofDatetime?)) #:index labels)]
      [(eq? series-type 'DateSeries)
       (new-DateSeries (assert (sequence->list data) Listofdate?) #:index labels)]
      [else
       (new-GenSeries (list->vector (sequence->list data)) #:index labels)])))

(: series-complete (SeriesBuilder -> Series))
(define (series-complete builder)
  (cond    
    ((NSeriesBuilder? builder)
     (complete-NSeriesBuilder builder))
    ((CSeriesBuilder? builder)
     (complete-CSeriesBuilder builder))
    ((ISeriesBuilder? builder)
     (complete-ISeriesBuilder builder))
    ((BSeriesBuilder? builder)
     (complete-BSeriesBuilder builder))
    ((DatetimeSeriesBuilder? builder)
     (complete-DatetimeSeriesBuilder builder))
    ((DateSeriesBuilder? builder)
     (complete-DateSeriesBuilder builder))
    (else
     (complete-GenSeriesBuilder builder))))

(: series-set-index (Series (U (Listof IndexDataType) RFIndex) -> Series))
(define (series-set-index series labels)
  (cond                                                      
    [(CSeries? series)
     (set-CSeries-index series labels)]
    [(NSeries? series)
     (set-NSeries-index series labels)]
    [(ISeries? series)
     (set-ISeries-index series labels)]
    [(BSeries? series)
     (set-BSeries-index series labels)]
    [(DatetimeSeries? series)
     (set-DatetimeSeries-index series labels)]
    [(DateSeries? series)
     (set-DateSeries-index series labels)]
    [else
     (set-GenSeries-index series labels)]))

(: series-set-null-value (Series GenericType -> Series))
(define (series-set-null-value series null-value)
  (cond                                                      
    [(CSeries? series)
     (set-CSeries-null-value series (assert null-value symbol?))]
    [(NSeries? series)
     (set-NSeries-null-value series (assert null-value flonum?))]
    [(ISeries? series)
     (set-ISeries-null-value series (assert null-value fixnum?))]
    [(BSeries? series)
     (set-BSeries-null-value series (assert null-value boolean?))]
    [(DatetimeSeries? series)
     (set-DatetimeSeries-null-value series (assert null-value Datetime?))]
    [(DateSeries? series)
     (set-DateSeries-null-value series (assert null-value date?))]
    [else
     (set-GenSeries-null-value series null-value)]))

(: get-series-null-value (Series -> GenericType))
(define (get-series-null-value series)
  (cond                                                      
    [(CSeries? series)
     (cseries-null-value series)]
    [(NSeries? series)
     (nseries-null-value series)]
    [(ISeries? series)
     (iseries-null-value series)]
    [(BSeries? series)
     (bseries-null-value series)]
    [(DatetimeSeries? series)
     (datetime-series-null-value series)]
    [(DateSeries? series)
     (date-series-null-value series)]
    [else
     (gen-series-null-value series)]))

(define-type GroupHash (U gen-series-grouphash cseries-grouphash nseries-grouphash iseries-grouphash bseries-grouphash datetime-series-grouphash date-series-grouphash))

; This function is self-explanatory, it consumes no arguments
; and creates a hash map which will represent a JoinHash.
(: make-group-hash (-> GroupHash))
(define (make-group-hash)
  (make-hash))

(: series-groupby (Series [#:by-value Boolean] -> GroupHash))
(define (series-groupby series #:by-value [by-value #f])
  (cond                                                      
    [(CSeries? series)
     (cseries-groupby series #:by-value by-value)]
    [(NSeries? series)
     (nseries-groupby series #:by-value by-value)]
    [(ISeries? series)
     (iseries-groupby series #:by-value by-value)]
    [(BSeries? series)
     (bseries-groupby series #:by-value by-value)]
    [(DatetimeSeries? series)
     (datetime-series-groupby series #:by-value by-value)]
    [(DateSeries? series)
     (date-series-groupby series #:by-value by-value)]
    [else
     (gen-series-groupby series #:by-value by-value)]))
#|
(define-type CSeriesFn (Label -> Label))
(define-predicate CSeriesFn? (Label -> Label))
(define-type NSeriesFn (Flonum -> Flonum))
(define-predicate NSeriesFn? NSeriesFn)
(define-type ISeriesFn (Fixnum -> Fixnum))
(define-predicate ISeriesFn? ISeriesFn)
(define-type BSeriesFn (Boolean -> Boolean))
(define-predicate BSeriesFn? BSeriesFn)
(define-type DatetimeSeriesFn (Datetime -> Datetime))
(define-predicate DatetimeSeriesFn? DatetimeSeriesFn)
(define-type DateSeriesFn (date -> date))
(define-predicate DateSeriesFn? DateSeriesFn)

(: series-map (Series (Any -> Any) -> Series))
(define (series-map series func)
  (cond                                                      
    [(CSeries? series)
     (cseries-map series (assert func CSeriesFn?))]
    [(NSeries? series)
     (map/ns series (assert func NSeriesFn?))]
    [(ISeries? series)
     (map/is series (assert func ISeriesFn?))]
    [(BSeries? series)
     (map/bs series (assert func BSeriesFn?))]
    [(DatetimeSeries? series)
     (map/datetime-series-data series (assert func DatetimeSeriesFn?))]
    [(DateSeries? series)
     (map/date-series-data series (assert func DateSeriesFn?))]
    [else
     (map/gen-s series func)])) |#

(: series-length (Series -> Index))
(define (series-length series)
    (cond                                                      
    [(CSeries? series)
     (cseries-length series)]
    [(NSeries? series)
     (nseries-length series )]
    [(ISeries? series)
     (iseries-length series)]
    [(BSeries? series)
     (bseries-length series)]
    [(DatetimeSeries? series)
     (datetime-series-length series)]
    [(DateSeries? series)
     (date-series-length series)]
    [else
     (gen-series-length series)]))

; ***********************************************************
; Get series data

(: series-data (Series -> (U (Vectorof GenericType) FlVector (Vectorof Symbol)
                             (Vectorof RFFixnum) (Vectorof Boolean) (Vectorof RFDatetime) (Vectorof RFDate))))
(define (series-data series)
  (cond
    [(GenSeries? series) (gen-series-data series)]
    [(NSeries? series) (nseries-data series)]    
    [(CSeries? series) (cseries-data series)]    
    [(ISeries? series) (iseries-data series)]
    [(BSeries? series) (bseries-data series)]
    [(DatetimeSeries? series) (datetime-series-data series)]
    [(DateSeries? series) (date-series-data series)]
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
    [(DateSeries? series) (date-series-iref series (list idx))]
    [else (error "Unknown Series type in DataFrame")]))

(: series-referencer (Series -> (-> Index Any)))
(define (series-referencer series)
  (cond
    [(GenSeries? series) (gen-series-referencer series)]
    [(NSeries? series) (nseries-referencer series)]    
    [(CSeries? series) (cseries-referencer series)]    
    [(ISeries? series) (iseries-referencer series)]
    [(BSeries? series) (bseries-referencer series)]
    [(DatetimeSeries? series) (datetime-series-referencer series)]
    [(DateSeries? series) (date-series-referencer series)]
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
    [(DateSeries? series) (date-series-index-ref series idx)]
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
    [(DateSeries? series) (date-series-loc-boolean series boolean-lst)]
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
    [(DateSeries? series) (date-series-loc series label)]
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
    [(DateSeries? series) (date-series-loc-multi-index series label)]
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
    [(DateSeries? series) (date-series-iloc series idx)]
    [else (error "Unknown Series type in DataFrame")]))

; ***********************************************************

; ***********************************************************

(: set-series-index (Series (U (Listof IndexDataType) RFIndex) -> Series))
(define (set-series-index series index)
  (cond
    [(GenSeries? series) (set-GenSeries-index (assert series GenSeries?) index)]
    [(NSeries? series) (set-NSeries-index (assert series NSeries?) index)]
    [(CSeries? series) (set-CSeries-index (assert series CSeries?) index)]    
    [(ISeries? series) (set-ISeries-index (assert series ISeries?) index)]
    [(BSeries? series) (set-BSeries-index (assert series BSeries?) index)]
    [(DatetimeSeries? series) (set-DatetimeSeries-index (assert series DatetimeSeries?) index)]
    [(DateSeries? series) (set-DateSeries-index (assert series DateSeries?) index)]
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
    [(DateSeries? series) (assert (date-series-index (assert series DateSeries?)))]
    [else (error "Unknown or not supported series type in DataFrame")]))

(: has-series-index? (Series -> Boolean))
(define (has-series-index? series)
  (cond
    [(GenSeries? series) (if (gen-series-index (assert series GenSeries?)) #t #f)]
    [(NSeries? series) (if (nseries-index (assert series NSeries?)) #t #f)]
    [(CSeries? series) (if (cseries-index (assert series CSeries?)) #t #f)]
    [(ISeries? series) (if (iseries-index (assert series ISeries?)) #t #f)]
    [(BSeries? series) (if (bseries-index (assert series BSeries?)) #t #f)]
    [(DatetimeSeries? series) (if (datetime-series-index (assert series DatetimeSeries?)) #t #f)]
    [(DateSeries? series) (if (date-series-index (assert series DateSeries?)) #t #f)]
    [else (error "Unknown or not supported series type in DataFrame")]))

; ***********************************************************
(define-type SeriesDataVectorType (U (Vectorof Any) (Vectorof Boolean) (Vectorof RFDatetime) (Vectorof RFFixnum) (Vectorof Symbol) (Vectorof RFDate) FlVector))
(: series-data->indexable-sequence (SeriesDataVectorType -> (Sequenceof IndexDataType)))
(define (series-data->indexable-sequence vectorof-any)
  (let ((indexable-sequence : (Sequenceof IndexDataType)
                            (if (flvector? vectorof-any)
                                (flvector->list (assert vectorof-any flvector?))
                                (assert (vector->list vectorof-any) ListofIndexDataType?))))
    indexable-sequence))
        
(: indexable-series->index (IndexableSeries -> RFIndex))
(define (indexable-series->index series)
  (build-index-from-sequence (series-data->indexable-sequence (series-data series))))

(: in-series (GenericType Series -> Boolean))
(define (in-series val series)
  (cond
    [(GenSeries? series) (in-gen-series val (assert series GenSeries?))]
    [(NSeries? series) (in-nseries (assert val flonum?) (assert series NSeries?))]
    [(CSeries? series) (in-cseries (assert val symbol?) series)]   
    [(ISeries? series) (in-iseries (assert val RFFixnum?) (assert series ISeries?))]
    [(BSeries? series) (in-bseries (assert val boolean?) (assert series BSeries?))]
    [(DatetimeSeries? series) (in-datetime-series (assert val Datetime?) (assert series DatetimeSeries?))]
    [(DateSeries? series) (in-date-series (assert val date?) (assert series DateSeries?))]
    [else (error "Unknown or not supported series type in DataFrame")]))

;define-type does not deliver this level of granularity when it comes to procedures. Even moreover, the brief false hope that we might easily wring such type differentiation for procedures manually using define-predicate is dashed by:
;Evaluates to a predicate for the type t, with the type (Any -> Boolean : t). t may not contain function types, or types that may refer to mutable data such as (Vectorof Integer). So we need to do assert checks in filter procedure.

;(define-type Gen->Bool (GenericType -> Boolean))
;(define-type RFFixnum->Bool (RFFixnum -> Boolean))
;(define-predicate RFFixnum->Bool? RFFixnum->Bool)
(: series-filter ((GenericType -> Boolean) Series -> Series))
(define (series-filter filter-procedure series)
  (cond
    [(GenSeries? series) (gen-series-filter (assert series GenSeries?) filter-procedure)]
    [(NSeries? series) (nseries-filter (assert series NSeries?) filter-procedure)]
    [(CSeries? series) (cseries-filter (assert series CSeries?) filter-procedure)]   
    [(ISeries? series) (iseries-filter (assert series ISeries?) filter-procedure)]
    ;[(BSeries? series) (in-bseries (assert val boolean?) (assert series BSeries?))]
    ;[(DatetimeSeries? series) (in-datetime-series (assert val Datetime?) (assert series DatetimeSeries?))]
    ;[(DateSeries? series) (in-date-series (assert val date?) (assert series DateSeries?))]
    [else (error "Unknown or not supported series type in DataFrame")]))

(series-filter (lambda ([x : GenericType]) (even? (assert x fixnum?))) (new-series (list 1 2 3 4 5)))

(series-filter (lambda ([x : GenericType]) (> (assert x flonum?) 2)) (new-series (list 1 2 3.2 4 5.8)))

; FUTURE WORK ;
;(: re-align-series-index (Series RFIndex))
; loop through series and grab positional index and use idx->key function to get the matching index value
; this is especially useful if series has been modified or filtered and the new index needs to be set
; accordingly
