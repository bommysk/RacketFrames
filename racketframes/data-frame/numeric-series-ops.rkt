#lang typed/racket

(provide:
 [nseries-head-display (NSeries [#:rows Index] -> Void)]
 [nseries-unique (NSeries -> NSeries)] 
 [nseries-append (NSeries NSeries -> NSeries)]
 [nseries-abs (NSeries -> NSeries)])

(require
 racket/flonum
 (only-in "../util/format.rkt"
	  ~a)
 (only-in racket/vector
	  vector-copy)
 (only-in "numeric-series.rkt"
	  NSeries new-NSeries nseries-data nseries-length nseries-referencer nseries-iref flvector->list list->flvector)
 (only-in "boolean-series.rkt"
	  BSeries new-BSeries bseries-data bseries-length bseries-referencer bseries-iref)
 (only-in "numeric-series-builder.rkt"
	  NSeriesBuilder
	  new-NSeriesBuilder
	  append-NSeriesBuilder
	  complete-NSeriesBuilder))

(: nseries-append (NSeries NSeries -> NSeries))
(define (nseries-append nsa nsb)

  (define builder (new-NSeriesBuilder (assert (+ (nseries-length nsa)
						 (nseries-length nsb))
					      index?)))

  (: append-nseries (NSeries -> Void))
  (define (append-nseries ns)
    (define ns-cnt (nseries-length ns))
    (define nref (nseries-referencer ns))
    (do ([i 0 (add1 i)])
	((>= i ns-cnt))
      (append-NSeriesBuilder builder (nref (assert i index?)))))
  
  (append-nseries nsa)
  (append-nseries nsb)
  (complete-NSeriesBuilder builder))

(: remove-duplicates ((Listof Float) -> (Listof Float)))
(define (remove-duplicates lst)
  (foldr (lambda ([x : Float] [y : (Listof Float)]) (cons x (filter (lambda ([z : Float]) (not (= x z))) y))) null lst))

(: nseries-unique (NSeries -> NSeries))
(define (nseries-unique nseries)
  (new-NSeries (list->flvector (remove-duplicates (flvector->list (nseries-data nseries))))))

(define default-nseries-rows 10)

(: nseries-head (NSeries [#:rows Index] -> NSeries))
(define (nseries-head nseries #:rows [rows default-nseries-rows])
  (define nref (nseries-referencer nseries))
  (let ((rows (min rows (nseries-length nseries))))
    (define data (make-flvector rows))
    (for ([i rows])
      (flvector-set! data i (nref (assert i index?))))
    (new-NSeries data)))

(: nseries-head-display (NSeries [#:rows Index] -> Void))
(define (nseries-head-display nseries #:rows [rows default-nseries-rows])
  (define nref (nseries-referencer nseries))
  (let ((rows (min rows (nseries-length nseries))))
    (do ([i 0 (add1 i)])
	((>= i rows) (displayln ""))
      (display (~a (string-append "[" (number->string i) "]") 
		   #:width 5 #:align 'left))
      (displayln (~a (number->string (nref (assert i index?))) 
		     #:align 'left)))))

(: nseries-abs (NSeries -> NSeries))
(define (nseries-abs nseries)
  (define nref (nseries-referencer nseries))
  (define rows (nseries-length nseries))
  (define builder (new-NSeriesBuilder rows))

  (for ([i rows])
    (append-NSeriesBuilder builder (abs (nref (assert i index?)))))

  (complete-NSeriesBuilder builder))

; Series.isna()	Return a boolean same-sized object indicating if the values are NA.
(: nseries-isna (NSeries -> BSeries))
(define (nseries-isna nseries)
  (define nref (nseries-referencer nseries))
  (let ((rows (nseries-length nseries)))
    (define data : (Vectorof Boolean) (make-vector (nseries-length nseries) #f))
    (for ([i rows])
      (if (= (nref (assert i index?)) -nan.0)
          (vector-set! data i #t)
          (vector-set! data i #f)))
    (new-BSeries data)))


; Series.notna() Return a boolean same-sized object indicating if the values are not NA.
(: nseries-notna (NSeries -> BSeries))
(define (nseries-notna nseries)
  (define nref (nseries-referencer nseries))
  (let ((rows (nseries-length nseries)))
    (define data : (Vectorof Boolean) (make-vector (nseries-length nseries) #f))
    (for ([i rows])
      (if (= (nref (assert i index?)) -nan.0)
          (vector-set! data i #t)
          (vector-set! data i #f)))
    (new-BSeries data)))

(define N (expt 10 4))
(: col-d-data FlVector)
(define col-d-data (make-flvector N))
