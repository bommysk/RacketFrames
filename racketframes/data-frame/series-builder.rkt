#lang typed/racket

(provide
 SeriesBuilder)

(require
  (only-in "generic-series-builder.rkt"
           GenSeriesBuilder)
  (only-in "numeric-series-builder.rkt"
           NSeriesBuilder)
  (only-in "categorical-series-builder.rkt"
           CSeriesBuilder)
  (only-in "integer-series-builder.rkt"
           ISeriesBuilder)
  (only-in "boolean-series-builder.rkt"
           BSeriesBuilder)
  (only-in "datetime-series-builder.rkt"
           DatetimeSeriesBuilder)
  (only-in "date-series-builder.rkt"
           DateSeriesBuilder))

(define-type SeriesBuilder (U GenSeriesBuilder ISeriesBuilder CSeriesBuilder NSeriesBuilder BSeriesBuilder DatetimeSeriesBuilder DateSeriesBuilder))

