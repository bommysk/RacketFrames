#lang typed/racket

(provide:
 [series-print (Series [#:output-port Output-Port] -> Void)]
 [column-print (Column [#:output-port Output-Port] -> Void)])

(require
 (only-in "series-description.rkt"
	  Series)
 (only-in "generic-series.rkt"
          GenSeries? gen-series-print)
 (only-in "categorical-series.rkt"
	  CSeries? cseries-print)
 (only-in "numeric-series.rkt"
	  NSeries? nseries-print)
 (only-in "integer-series.rkt"
	  ISeries? iseries-print)
 (only-in "boolean-series.rkt"
	  BSeries? bseries-print)
 (only-in "datetime-series.rkt"
	  DatetimeSeries? datetime-series-print)
 (only-in "date-series.rkt"
	  DateSeries? date-series-print)
 (only-in "data-frame.rkt"
          Column column-heading column-series))


(: series-print (Series [#:output-port Output-Port] -> Void))
(define (series-print series #:output-port [port (current-output-port)])
  (cond
    ((GenSeries? series)
     (gen-series-print series))
    ((NSeries? series)
     (nseries-print series))
    ((CSeries? series)
     (cseries-print series))
    ((ISeries? series)
     (iseries-print series))
    ((BSeries? series)
     (bseries-print series))
    ((DatetimeSeries? series)
     (datetime-series-print series))
    ((DateSeries? series)
     (date-series-print series))
    [else (error "Unknown series type.")]))

(: column-print (Column [#:output-port Output-Port] -> Void))
(define (column-print column #:output-port [port (current-output-port)])
  (let ((heading (column-heading column))
        (series (column-series column)))
    (displayln heading)
    (series-print series #:output-port port)))
