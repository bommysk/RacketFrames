#lang typed/racket

(provide:
 [cseries-head (CSeries [#:rows Index] -> CSeries)]
 [cseries-head-display (CSeries [#:rows Index] -> Void)]
 [cseries-unique (CSeries -> CSeries)] 
 [cseries-append (CSeries CSeries -> CSeries)])

(require 
  (only-in racket/vector
           vector-copy)
  (only-in "../util/format.rkt"
           ~a)
  (only-in "categorical-series.rkt"
           CSeries new-CSeries cseries-data cseries-nominal-data cseries-index
           cseries-length cseries-referencer)
  (only-in "categorical-series-builder.rkt"
           CSeriesBuilder
           new-CSeriesBuilder
           append-CSeriesBuilder
           complete-CSeriesBuilder)
  (only-in "indexed-series.rkt"
           build-index-from-list))

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
  (new-CSeries (cseries-nominal-data cseries) #:index (cseries-index cseries)))
   
(define default-cseries-rows 10)

(: cseries-head (CSeries [#:rows Index] -> CSeries))
(define (cseries-head cseries #:rows [rows 10])
  (define cref (cseries-referencer cseries))
  (if (< (vector-length (cseries-data cseries)) rows)
      (new-CSeries (for/vector: : (Vectorof Symbol) ([i (vector-length (cseries-data cseries))]) (cref i)) #:index (build-index-from-list (vector->list (cseries-data cseries))))
      (new-CSeries (for/vector: : (Vectorof Symbol) ([i rows]) (cref i)) #:index (build-index-from-list (vector->list (cseries-data cseries))))))

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
