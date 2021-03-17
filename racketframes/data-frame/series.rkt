#lang typed/racket

(provide:
 [new-series ((Sequenceof Any) (Option (U (Listof IndexDataType) RFIndex)) -> Series)]
 [set-series-index (Series (U (Listof IndexDataType) RFIndex) -> Series)]
 [series-set-null-value (Series GenericType -> Series)]
 [series-groupby (Series [#:by-value Boolean] -> GroupHash)]
 [series-complete (SeriesBuilder -> Series)])

(require
  racket/fixnum
  racket/flonum
  (only-in "indexed-series.rkt"
          Label RFIndex IndexDataType ListofFlonum? ListofFixnum? ListofBoolean? ListofDatetime? ListofLabel?)
 (only-in "series-description.rkt"
	  Series SeriesType series-type)
 (only-in "groupby-util.rkt" gen-series-groupby)
 (only-in "generic-series.rkt"
          new-GenSeries GenSeries? set-GenSeries-index)
 (only-in "categorical-series.rkt"
          new-CSeries CSeries? cseries-index set-CSeries-index set-CSeries-null-value cseries-groupby)
 (only-in "numeric-series.rkt"
          new-NSeries list->flvector NSeries? set-NSeries-index set-NSeries-null-value nseries-groupby)
 (only-in "integer-series.rkt"
          new-ISeries ISeries? set-ISeries-index set-ISeries-null-value iseries-groupby)
 (only-in "boolean-series.rkt"
          new-BSeries BSeries? set-BSeries-index set-BSeries-null-value bseries-groupby)
 (only-in "datetime-series.rkt"
          new-DatetimeSeries DatetimeSeries? set-DatetimeSeries-index set-DatetimeSeries-null-value datetime-series-groupby)
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
 (only-in "../load/sample.rkt"
          guess-series-type))

(: new-series ((Sequenceof Any) (Option (U (Listof IndexDataType) RFIndex)) -> Series))
(define (new-series data labels)
  (let*: ((series-type : SeriesType (guess-series-type (map ~a (sequence->list data)))))
    (cond                                                      
      [(eq? series-type 'CategoricalSeries)
       (new-CSeries (list->vector (assert (sequence->list data) ListofLabel?)) labels)]
      [(eq? series-type 'NumericSeries)
       (new-NSeries (list->flvector (assert (sequence->list data) ListofFlonum?)) labels)]
      [(eq? series-type 'IntegerSeries)
       (new-ISeries (list->vector (assert (sequence->list data) ListofFixnum?)) labels)]
      [(eq? series-type 'BooleanSeries)
       (new-BSeries (list->vector (assert (sequence->list data) ListofBoolean?)) labels)]
      [(eq? series-type 'DatetimeSeries)
       (new-DatetimeSeries (list->vector (assert (sequence->list data) ListofDatetime?)) labels)]
      [else
       (new-GenSeries (list->vector (sequence->list data)) labels)])))

(: series-complete (SeriesBuilder -> Series))
(define (series-complete builder)
  (cond
    ((GenSeriesBuilder? builder)
     (complete-GenSeriesBuilder builder))
    ((NSeriesBuilder? builder)
     (complete-NSeriesBuilder builder))
    ((CSeriesBuilder? builder)
     (complete-CSeriesBuilder builder))
    ((ISeriesBuilder? builder)
     (complete-ISeriesBuilder builder))
    ((BSeriesBuilder? builder)
     (complete-BSeriesBuilder builder))
    ((DatetimeSeriesBuilder? builder)
     (complete-DatetimeSeriesBuilder builder))))

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
    [else
     (set-GenSeries-null-value series null-value)]))

(: get-series-null-value (Series -> GenericType))
(define (set-series-index series null-value)
  (cond                                                      
    [(CSeries? series)
     (cseries-null-value series (assert null-value symbol?))]
    [(NSeries? series)
     (set-NSeries-null-value series (assert null-value flonum?))]
    [(ISeries? series)
     (set-ISeries-null-value series (assert null-value fixnum?))]
    [(BSeries? series)
     (set-BSeries-null-value series (assert null-value boolean?))]
    [(DatetimeSeries? series)
     (set-DatetimeSeries-null-value series (assert null-value Datetime?))]
    [else
     (set-GenSeries-null-value series null-value)]))

(define-type Key String)
(define-type GroupHash (HashTable Key (Listof GenericType)))

; This function is self-explanatory, it consumes no arguments
; and creates a hash map which will represent a JoinHash.
(: make-group-hash (-> GroupHash))
(define (make-group-hash)
  (make-hash))

(: series-groupby (Series [#:by-value Boolean] -> GroupHash))
(define (series-groupby series #:by-value [by-value #f])
  (cond                                                      
    [(CSeries? series)
     (cseries-groupby series by-value)]
    [(NSeries? series)
     (nseries-groupby series by-value)]
    [(ISeries? series)
     (iseries-groupby series by-value)]
    [(BSeries? series)
     (bseries-groupby series by-value)]
    [(DatetimeSeries? series)
     (datetime-series-groupby series by-value)]
    [else
     (gen-series-groupby series by-value)]))

;(: convert-GenSeries-to-OriginalSeries (GenSeries -> Series))