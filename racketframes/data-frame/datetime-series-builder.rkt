#lang typed/racket

(provide
 (struct-out DatetimeSeriesBuilder))

(provide:
 [new-DatetimeSeriesBuilder (case-> 
		      (-> DatetimeSeriesBuilder)
		      (Index -> DatetimeSeriesBuilder))]
 [append-DatetimeSeriesBuilder   (DatetimeSeriesBuilder (U Datetime String) -> Void)]
 [complete-DatetimeSeriesBuilder (DatetimeSeriesBuilder -> DatetimeSeries)])

(require
 racket/fixnum
 (only-in racket/vector
	  vector-copy)
 (only-in "datetime-series.rkt"
          DatetimeSeries new-DatetimeSeries)
 (only-in "../util/datetime/parse.rkt"
          parse-date parse-datetime is-valid-date? is-valid-datetime?)
 (only-in "../util/datetime/types.rkt"
          Datetime Datetime? Date Time))

(struct: DatetimeSeriesBuilder ([index  : Index]
                                [data : (Vectorof Datetime)]) #:mutable)

(define base-len 512)

(: new-DatetimeSeriesBuilder (case-> 
		       (-> DatetimeSeriesBuilder)
		       (Index -> DatetimeSeriesBuilder)))
(define (new-DatetimeSeriesBuilder [len base-len])
  (DatetimeSeriesBuilder 0 (make-vector len (Datetime (Date 0 0 0) (Time 0 0 0 0 0)))))

(: append-DatetimeSeriesBuilder (DatetimeSeriesBuilder (U Datetime String) -> Void))
(define (append-DatetimeSeriesBuilder builder datetime/str-value)
  
  (define-syntax bump
    (syntax-rules ()
      [(bump x)
       (assert (add1 x) index?)]))
  
  (define (bump-index)
    (let ((idx (DatetimeSeriesBuilder-index builder)))
      (set-DatetimeSeriesBuilder-index! builder (bump idx))
      idx))
  
  (: extend-data (-> Void))
  (define (extend-data)
    (let* ((data (DatetimeSeriesBuilder-data builder))
	   (curr-len (vector-length data))
	   (new-len  (assert (inexact->exact (round (* 1.5 curr-len))) exact-integer?)))
      (let: ((new-data : (Vectorof Datetime) ((inst make-vector Datetime) new-len (Datetime (Date 0 0 0) (Time 0 0 0 0 0)))))
	    (do ([idx 0 (add1 idx)])
		([>= idx curr-len] (set-DatetimeSeriesBuilder-data! builder new-data))
	      (vector-set! new-data idx (vector-ref data idx))))))
  
  (if (< (DatetimeSeriesBuilder-index builder)         
         (vector-length (DatetimeSeriesBuilder-data builder)))
      (let ((dt (if (string? datetime/str-value)            
		     (let ((dt (cond
                                 [(is-valid-datetime? datetime/str-value) (parse-datetime (string-trim datetime/str-value))]
                                 [(is-valid-date? datetime/str-value) (parse-date (string-trim datetime/str-value))]                                 
                                 [else #f])))                       
                       (if dt (assert dt Datetime?) (Datetime (Date 0 0 0) (Time 0 0 0 0 0))))
		     datetime/str-value)))
        (vector-set! (DatetimeSeriesBuilder-data builder)
		     (bump-index)
		     dt))
      (begin
        (extend-data)       
        (append-DatetimeSeriesBuilder builder datetime/str-value))))

(: complete-DatetimeSeriesBuilder (DatetimeSeriesBuilder -> DatetimeSeries))
(define (complete-DatetimeSeriesBuilder builder)  
  (let* ((data (DatetimeSeriesBuilder-data builder))
         (len (DatetimeSeriesBuilder-index builder)))
    (new-DatetimeSeries (vector-copy data 0 len))))
