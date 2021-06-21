#lang typed/racket

(provide
 (struct-out DateSeriesBuilder))

(provide:
 [new-DateSeriesBuilder (case-> 
		      (-> DateSeriesBuilder)
		      (Index -> DateSeriesBuilder))]
 [append-DateSeriesBuilder   (DateSeriesBuilder (U RFDate String) -> Void)]
 [complete-DateSeriesBuilder (DateSeriesBuilder -> DateSeries)])

(require typed/racket/date)

(require
 racket/fixnum
 (only-in racket/vector
	  vector-copy)
 (only-in "date-series.rkt"
          DateSeries new-DateSeries RFDate DATE_SERIES_DEFAULT_NULL_VALUE)
 (only-in "../util/datetime/parse.rkt"
          parse-racket-date parse-racket-datetime is-valid-date? is-valid-datetime?))

(struct: DateSeriesBuilder ([index  : Index]
                            [data : (Vectorof RFDate)]) #:mutable)

(define base-len 512)

(: new-DateSeriesBuilder (case-> 
		       (-> DateSeriesBuilder)
		       (Index -> DateSeriesBuilder)))
(define (new-DateSeriesBuilder [len base-len])
  (DateSeriesBuilder 0 (make-vector len DATE_SERIES_DEFAULT_NULL_VALUE)))

;(define-type DateParseFormat (U 'date 'time 'datetime 'moment))

(: append-DateSeriesBuilder (DateSeriesBuilder (U RFDate String) -> Void))
(define (append-DateSeriesBuilder builder rfdate/str-value)
  
  (define-syntax bump
    (syntax-rules ()
      [(bump x)
       (assert (add1 x) index?)]))
  
  (define (bump-index)
    (let ((idx (DateSeriesBuilder-index builder)))
      (set-DateSeriesBuilder-index! builder (bump idx))
      idx))
  
  (: extend-data (-> Void))
  (define (extend-data)
    (let* ((data (DateSeriesBuilder-data builder))
	   (curr-len (vector-length data))
	   (new-len  (assert (inexact->exact (round (* 1.5 curr-len))) exact-integer?)))
      (let: ((new-data : (Vectorof RFDate) ((inst make-vector RFDate) new-len DATE_SERIES_DEFAULT_NULL_VALUE)))
	    (do ([idx 0 (add1 idx)])
		([>= idx curr-len] (set-DateSeriesBuilder-data! builder new-data))
	      (vector-set! new-data idx (vector-ref data idx))))))
  
  (if (< (DateSeriesBuilder-index builder)         
         (vector-length (DateSeriesBuilder-data builder)))
      (let ((dt (if (string? rfdate/str-value)            
		     (let ((dt (cond
                                 [(is-valid-datetime? rfdate/str-value) (parse-racket-datetime (string-trim rfdate/str-value))]
                                 [(is-valid-date? rfdate/str-value) (parse-racket-date (string-trim rfdate/str-value))]                                 
                                 [else #f])))
                       (if dt (assert dt date?) DATE_SERIES_DEFAULT_NULL_VALUE))
		     rfdate/str-value)))
        (vector-set! (DateSeriesBuilder-data builder)
		     (bump-index)
		     dt))
      (begin
        (extend-data)       
        (append-DateSeriesBuilder builder rfdate/str-value))))

(: complete-DateSeriesBuilder (DateSeriesBuilder -> DateSeries))
(define (complete-DateSeriesBuilder builder)  
  (let* ((data (DateSeriesBuilder-data builder))
         (len (DateSeriesBuilder-index builder)))
    (new-DateSeries (vector-copy data 0 len))))
