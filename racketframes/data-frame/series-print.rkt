#lang typed/racket

(provide:
 [series-print (Series [#:output-port Output-Port] [#:count (Option Index)] -> Void)]
 [column-print (Column [#:output-port Output-Port] [#:count (Option Index)] -> Void)])

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


(: series-print (Series [#:output-port Output-Port] [#:count (Option Index)] -> Void))
(define (series-print series #:output-port [port (current-output-port)] #:count [count #f])
  (cond
    ((GenSeries? series)
     (gen-series-print series #:output-port port #:count count))
    ((NSeries? series)
     (nseries-print series #:output-port port #:count count))
    ((CSeries? series)
     (cseries-print series #:output-port port #:count count))
    ((ISeries? series)
     (iseries-print series #:output-port port #:count count))
    ((BSeries? series)
     (bseries-print series #:output-port port #:count count))
    ((DatetimeSeries? series)
     (datetime-series-print series #:output-port port #:count count))
    ((DateSeries? series)
     (date-series-print series #:output-port port #:count count))
    [else (error "Unknown series type.")]))

(: column-print (Column [#:output-port Output-Port] [#:count (Option Index)] -> Void))
(define (column-print column #:output-port [port (current-output-port)] #:count [count #f])
  (let ((heading (column-heading column))
        (series (column-series column)))
    (displayln heading)
    (series-print series #:output-port port #:count count)))
