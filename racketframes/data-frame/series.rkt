#lang typed/racket

(provide:
 [new-series ((Sequenceof Any) (Option (U (Listof IndexDataType) RFIndex)) -> Series)]
 [series-complete (SeriesBuilder -> Series)])

(require
  (only-in "indexed-series.rkt"
          Label RFIndex IndexDataType ListofFlonum? ListofFixnum? ListofBoolean? ListofDatetime? ListofLabel?)
 (only-in "series-description.rkt"
	  Series SeriesType)
 (only-in "generic-series.rkt" new-GenSeries)
 (only-in "categorical-series.rkt" new-CSeries)
 (only-in "numeric-series.rkt" new-NSeries list->flvector)
 (only-in "integer-series.rkt" new-ISeries)
 (only-in "boolean-series.rkt" new-BSeries)
 (only-in "datetime-series.rkt" new-DatetimeSeries)
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



