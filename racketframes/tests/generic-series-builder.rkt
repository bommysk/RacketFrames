#lang typed/racket

(provide
 (struct-out GenSeriesBuilder))

(provide:
 [new-GenSeriesBuilder (case-> 
		      (-> GenSeriesBuilder)
		      (Index -> GenSeriesBuilder))]
 [append-GenSeriesBuilder   (GenSeriesBuilder Any -> Void)]
 [complete-GenSeriesBuilder (GenSeriesBuilder -> GenSeries)])

(require
 racket/fixnum
 (only-in racket/vector
	  vector-copy)
 (only-in "generic-series.rkt"
          GenSeries))

(struct: GenSeriesBuilder ([index  : Index]
                           [data : (Vectorof Any)]) #:mutable)

(define base-len 512)

(: string-to-symbol-or-num (String -> (U Number Symbol)))
(define (string-to-symbol-or-num str)
  (let ((num (string->number str)))
    (if num num (string->symbol str))))

(: new-GenSeriesBuilder (case-> 
		       (-> GenSeriesBuilder)
		       (Index -> GenSeriesBuilder)))
(define (new-GenSeriesBuilder [len base-len])
  (GenSeriesBuilder 0 (make-vector len 0)))

(: append-GenSeriesBuilder (GenSeriesBuilder Any -> Void))
(define (append-GenSeriesBuilder builder value)
  
  (define-syntax bump
    (syntax-rules ()
      [(bump x)
       (assert (add1 x) index?)]))
  
  (define (bump-index)
    (let ((idx (GenSeriesBuilder-index builder)))
      (set-GenSeriesBuilder-index! builder (bump idx))
      idx))
  
  (: extend-data (-> Void))
  (define (extend-data)
    (let* ((data (GenSeriesBuilder-data builder))
	   (curr-len (vector-length data))
	   (new-len  (assert (inexact->exact (round (* 1.5 curr-len))) exact-integer?)))
      (let: ((new-data : (Vectorof Any) ((inst make-vector Any) new-len 0)))
	    (do ([idx 0 (add1 idx)])
		([>= idx curr-len] (set-GenSeriesBuilder-data! builder new-data))
	      (vector-set! new-data idx (vector-ref data idx))))))
  
  (if (< (GenSeriesBuilder-index builder)         
         (vector-length (GenSeriesBuilder-data builder)))
        (let ((new-value (if (string? value)
		     (string-to-symbol-or-num value)
		     value)))
          (vector-set! (GenSeriesBuilder-data builder)
		     (bump-index)
                     new-value))
      (begin
        (extend-data)       
        (append-GenSeriesBuilder builder value))))

(: complete-GenSeriesBuilder (GenSeriesBuilder -> GenSeries))
(define (complete-GenSeriesBuilder builder)  
  (let* ((data (GenSeriesBuilder-data builder))
         (len (GenSeriesBuilder-index builder)))
    (GenSeries #f (vector-copy data 0 len))))
