#lang typed/racket

(provide
 (struct-out DateSeriesBuilder))

(provide:
 [new-DateSeriesBuilder (case-> 
		      (-> DateSeriesBuilder)
		      (Index -> DateSeriesBuilder))]
 [append-DateSeriesBuilder   (DateSeriesBuilder (U date String) -> Void)]
 [complete-DateSeriesBuilder (DateSeriesBuilder -> DateSeries)])

(require typed/racket/date)

(require
 racket/fixnum
 (only-in racket/vector
	  vector-copy)
 (only-in "date-series.rkt"
          DateSeries new-DateSeries DEFAULT_NULL_VALUE)
 (only-in "../util/datetime/parse.rkt"
          parse-racket-date parse-racket-datetime is-valid-date? is-valid-datetime?))

(struct: DateSeriesBuilder ([index  : Index]
                            [data : (Vectorof date)]) #:mutable)

(define base-len 512)

(: new-DateSeriesBuilder (case-> 
		       (-> DateSeriesBuilder)
		       (Index -> DateSeriesBuilder)))
(define (new-DateSeriesBuilder [len base-len])
  (DateSeriesBuilder 0 (make-vector len DEFAULT_NULL_VALUE)))

;(define-type DateParseFormat (U 'date 'time 'datetime 'moment))

(: append-DateSeriesBuilder (DateSeriesBuilder (U date String) -> Void))
(define (append-DateSeriesBuilder builder date/str-value)
  
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
      (let: ((new-data : (Vectorof date) ((inst make-vector date) new-len DEFAULT_NULL_VALUE)))
	    (do ([idx 0 (add1 idx)])
		([>= idx curr-len] (set-DateSeriesBuilder-data! builder new-data))
	      (vector-set! new-data idx (vector-ref data idx))))))
  
  (if (< (DateSeriesBuilder-index builder)         
         (vector-length (DateSeriesBuilder-data builder)))
      (let ((dt (if (string? date/str-value)            
		     (let ((dt (cond
                                 [(is-valid-datetime? date/str-value) (parse-racket-datetime (string-trim date/str-value))]
                                 [(is-valid-date? date/str-value) (parse-racket-date (string-trim date/str-value))]                                 
                                 [else #f])))
                       (if dt (assert dt date?) DEFAULT_NULL_VALUE))
		     date/str-value)))
        (vector-set! (DateSeriesBuilder-data builder)
		     (bump-index)
		     dt))
      (begin
        (extend-data)       
        (append-DateSeriesBuilder builder date/str-value))))

(: complete-DateSeriesBuilder (DateSeriesBuilder -> DateSeries))
(define (complete-DateSeriesBuilder builder)  
  (let* ((data (DateSeriesBuilder-data builder))
         (len (DateSeriesBuilder-index builder)))
    (new-DateSeries (vector-copy data 0 len))))
