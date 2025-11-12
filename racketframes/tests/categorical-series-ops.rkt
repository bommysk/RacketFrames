#lang typed/racket

(provide:
 [cseries-head (CSeries [#:rows Index] -> CSeries)]
 [cseries-head-display (CSeries [#:rows Index] -> Void)]
 [cseries-unique (CSeries -> CSeries)] 
 [cseries-append (CSeries CSeries -> CSeries)])

(require
  (only-in "../util/format.rkt"
	  ~a)
 (only-in racket/vector
	  vector-copy)
 (only-in "categorical-series.rkt"
	  CSeries new-CSeries cseries-data CSeries-nominals cseries-length cseries-referencer)
 (only-in "categorical-series-builder.rkt"
	  CSeriesBuilder
	  new-CSeriesBuilder
	  append-CSeriesBuilder
	  complete-CSeriesBuilder))

(: cseries-append (CSeries CSeries -> CSeries))
(define (cseries-append csa csb)

  (define builder (new-CSeriesBuilder (assert (+ (cseries-length csa)
						 (cseries-length csb))
					      index?)))

  (: append-cseries (CSeries -> Void))
  (define (append-cseries cs)
    (define cs-cnt (cseries-length cs))
    (define cref (cseries-referencer cs))
    (do ([i 0 (add1 i)])
	((>= i cs-cnt))
      (append-CSeriesBuilder builder (cref (assert i index?)))))
  
  (append-cseries csa)
  (append-cseries csb)
  (complete-CSeriesBuilder builder))


(: cseries-unique (CSeries -> CSeries))
(define (cseries-unique cseries)
  (define noms (vector-copy (CSeries-nominals cseries)))
  
  (define len (vector-length noms))
  (define: data : (Vectorof Index) (make-vector len 0))

  (do ([i 0 (add1 i)])
      ((>= i len) (CSeries #f data noms))
    (let: ((i : Index (assert i index?)))
	  (vector-set! data i i))))

(define default-cseries-rows 10)

(: cseries-head (CSeries [#:rows Index] -> CSeries))
(define (cseries-head cseries #:rows [rows 10])
  (define cref (cseries-referencer cseries))
  (if (< (vector-length (cseries-data cseries)) rows)
      (new-CSeries (for/vector: : (Vectorof Symbol) ([i (vector-length (cseries-data cseries))]) (cref i)) #f)
      (new-CSeries (for/vector: : (Vectorof Symbol) ([i rows]) (cref i)) #f)))

(: cseries-head-display (CSeries [#:rows Index] -> Void))
(define (cseries-head-display cseries #:rows [rows default-cseries-rows])
  (define cref (cseries-referencer cseries))
  (let ((rows (min rows (cseries-length cseries))))
    (do ([i 0 (add1 i)])
	((>= i rows) (displayln ""))
      (display (~a (string-append "[" (number->string i) "]") 
		   #:width 5 #:align 'left))
      (displayln (~a (symbol->string (cref (assert i index?))) 
		     #:align 'left)))))
