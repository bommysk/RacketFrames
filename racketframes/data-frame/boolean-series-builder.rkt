#lang typed/racket

(provide
 (struct-out BSeriesBuilder))

(provide:
 [new-BSeriesBuilder (case-> 
		      (-> BSeriesBuilder)
		      (Index -> BSeriesBuilder))]
 [append-BSeriesBuilder   (BSeriesBuilder (U Boolean String) -> Void)]
 [complete-BSeriesBuilder (BSeriesBuilder -> BSeries)])

(require
 (only-in racket/vector
	  vector-copy)
 (only-in "boolean-series.rkt"
          new-BSeries BSeries))

(struct: BSeriesBuilder ([index  : Index]
			 [data : (Vectorof Boolean)]) #:mutable)

(define base-len 512)

(: new-BSeriesBuilder (case-> 
		       (-> BSeriesBuilder)
		       (Index -> BSeriesBuilder)))
(define (new-BSeriesBuilder [len base-len])
  (BSeriesBuilder 0 (make-vector len #f)))

; Handle case insensitive boolean values.
(: string->boolean (String -> Boolean))
(define (string->boolean str)
  (cond
    [(string-ci=? str "#t") #t]
    [(string-ci=? str "true") #t]
    [(string-ci=? str "#true") #t]
    [(string-ci=? str "#f") #f]
    [(string-ci=? str "false") #f]
    [(string-ci=? str "#false") #f]
    [else #f]))

(: append-BSeriesBuilder (BSeriesBuilder (U Boolean String) -> Void))
(define (append-BSeriesBuilder builder bool/str-value)
  
  (define-syntax bump
    (syntax-rules ()
      [(bump x)
       (assert (add1 x) index?)]))
  
  (define (bump-index)
    (let ((idx (BSeriesBuilder-index builder)))
      (set-BSeriesBuilder-index! builder (bump idx))
      idx))
  
  (: extend-data (-> Void))
  (define (extend-data)
    (let* ((data (BSeriesBuilder-data builder))
	   (curr-len (vector-length data))
	   (new-len  (assert (inexact->exact (round (* 1.5 curr-len))) exact-integer?)))
      (let: ((new-data : (Vectorof Boolean) ((inst make-vector Boolean) new-len #f)))
	    (do ([idx 0 (add1 idx)])
		([>= idx curr-len] (set-BSeriesBuilder-data! builder new-data))
	      (vector-set! new-data idx (vector-ref data idx))))))
  
  (if (< (BSeriesBuilder-index builder)         
         (vector-length (BSeriesBuilder-data builder)))

      (let ((bool (if (string? bool/str-value)
                      (let ((bool (string->boolean (string-trim bool/str-value))))
                        (if bool bool #f))
                      bool/str-value)))
      
        (vector-set! (BSeriesBuilder-data builder)
		     (bump-index)
		     bool))
      (begin
        (extend-data)       
        (append-BSeriesBuilder builder bool/str-value))))

(: complete-BSeriesBuilder (BSeriesBuilder -> BSeries))
(define (complete-BSeriesBuilder builder)  
  (let* ((data (BSeriesBuilder-data builder))
         (len (BSeriesBuilder-index builder)))
    (new-BSeries (vector-copy data 0 len) #f)))
