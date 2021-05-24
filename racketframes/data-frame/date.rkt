#lang typed/racket

(provide:
 [as-julian-day-numbers (CSeries -> ISeries)])

(require
 racket/fixnum
 (only-in "../util/datetime/convert.rkt"
          date->julian-day-number)
 (only-in "../util/datetime/parse.rkt"
	  parse-date)
 (only-in "categorical-series.rkt"
	  CSeries new-CSeries cseries-data
	  cseries-length cseries-referencer)
 (only-in "integer-series.rkt"
	  ISeries iseries-data
	  new-ISeries)
 (only-in "integer-series-builder.rkt"
	  new-ISeriesBuilder
	  append-ISeriesBuilder
	  complete-ISeriesBuilder)
  (only-in "../util/datetime/types.rkt"	  
	  Instant Instant? 
	  Datetime Datetime? Datetime-date Datetime-time
	  Date Date? Date-day Date-month Date-year
	  Time Time? Time-offset Time-hour Time-minute Time-second Time-milli
	  ))

(: as-julian-day-numbers (CSeries -> ISeries))
(define (as-julian-day-numbers cseries)
  (let* ((clen (cseries-length cseries))
	 (cref (cseries-referencer cseries))
	 (ibuilder (new-ISeriesBuilder clen)))
    (do: : ISeries ([idx : Index #{0 : Index} (assert (add1 idx) index?)])
	((= idx clen) (complete-ISeriesBuilder ibuilder))
      (let ((date (parse-date (symbol->string (cref idx)))))
	(if date
            (begin
              (displayln date)
              (displayln (date->julian-day-number (Datetime-date date)))
	    (append-ISeriesBuilder ibuilder (assert (date->julian-day-number (Datetime-date date)) fixnum?)))
	    (error 'as-julian-day-numbers "Invalid date: ~s" date))))))
