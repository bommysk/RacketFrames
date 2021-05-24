#lang typed/racket

(provide:
 [iseries-head (ISeries [#:rows Index] -> ISeries)]
 [iseries-unique (ISeries -> ISeries)] 
 [iseries-append (ISeries ISeries -> ISeries)]
 [iseries-abs (ISeries -> ISeries)])

(require
  (only-in racket/vector
           vector-copy)
  (only-in racket/format
           ~a)
  (only-in "integer-series.rkt"
           ISeries new-ISeries iseries-data iseries-length iseries-referencer iseries-iref
           iseries-index iseries-null-value derive-fixnum-value RFFixnum RFFixnum?)
  (only-in "boolean-series.rkt"
           BSeries new-BSeries bseries-data bseries-length bseries-referencer bseries-iref)
  (only-in "integer-series-builder.rkt"
           ISeriesBuilder
           new-ISeriesBuilder
           append-ISeriesBuilder
           complete-ISeriesBuilder))

(: iseries-append (ISeries ISeries -> ISeries))
(define (iseries-append isa isb)

  (define builder (new-ISeriesBuilder (assert (+ (iseries-length isa)
						 (iseries-length isb))
					      index?)))

  (: append-iseries (ISeries -> Void))
  (define (append-iseries is)
    (define is-cnt (iseries-length is))
    (define iref (iseries-referencer is))
    (do ([i 0 (add1 i)])
	((>= i is-cnt))
      (append-ISeriesBuilder builder (iref (assert i index?)))))
  
  (append-iseries isa)
  (append-iseries isb)
  (complete-ISeriesBuilder builder))

(: remove-duplicates-sequence ((Sequenceof RFFixnum) -> (Vectorof RFFixnum)))
(define (remove-duplicates-sequence fx-seq)  
  (let* ((data (remove-duplicates (sequence->list fx-seq)))
         (len (length data)))
    (let: ((new-data : (Vectorof RFFixnum) ((inst make-vector RFFixnum) len 0)))
      (do ([idx 0 (add1 idx)])
        ([>= idx len] new-data)
        (vector-set! new-data idx (list-ref data idx))))))

(: iseries-unique (ISeries -> ISeries))
(define (iseries-unique iseries)
  (new-ISeries (remove-duplicates-sequence (iseries-data iseries))
               #:index (iseries-index iseries)
               #:fill-null (iseries-null-value iseries)))

(: iseries-head (ISeries [#:rows Index] -> ISeries))
(define (iseries-head iseries #:rows [rows 10])
  (if (< (vector-length (iseries-data iseries)) rows)
      (new-ISeries (for/vector: : (Vectorof RFFixnum) ([i (vector-length (iseries-data iseries))]) (car (iseries-iref iseries (list i)))))
      (new-ISeries (for/vector: : (Vectorof RFFixnum) ([i rows]) (car (iseries-iref iseries (list i)))))))

(define default-iseries-rows 10)

(: display-iseries-head (ISeries [#:rows Index] -> Void))
(define (display-iseries-head iseries #:rows [rows default-iseries-rows])
  (define iref (iseries-referencer iseries))
  (let ((rows (min rows (iseries-length iseries))))
    (do ([i 0 (add1 i)])
	((>= i rows) (displayln ""))
      (display (~a (string-append "[" (number->string i) "]") 
		   #:width 5 #:align 'left))
      (displayln (~a (iref (assert i index?))
		     #:align 'left)))
    ))

(: iseries-abs (ISeries -> ISeries))
(define (iseries-abs iseries)
  (define iref (iseries-referencer iseries))
  (define rows (iseries-length iseries))
  (define builder (new-ISeriesBuilder rows))

  (for ([i rows])
    (append-ISeriesBuilder builder (assert (abs (derive-fixnum-value iseries (iref (assert i index?)))) RFFixnum?)))

  (complete-ISeriesBuilder builder))

; Series.isna()	Return a boolean same-sized object indicating if the values are NA.
(: iseries-isna (ISeries -> BSeries))
(define (iseries-isna iseries)
  (define iref (iseries-referencer iseries))
  (let ((rows (iseries-length iseries)))
    (define data : (Vectorof Boolean) (make-vector (iseries-length iseries) #f))
    (for ([i rows])
      (if (eq? (iref (assert i index?)) (iseries-null-value iseries))
          (vector-set! data i #t)
          (vector-set! data i #f)))
    (new-BSeries data)))

; Series.notna() Return a boolean same-sized object indicating if the values are not NA.
(: iseries-notna (ISeries -> BSeries))
(define (iseries-notna iseries)
  (define iref (iseries-referencer iseries))
  (let ((rows (iseries-length iseries)))
    (define data : (Vectorof Boolean) (make-vector (iseries-length iseries) #f))
    (for ([i rows])
      (if (eq? (iref (assert i index?)) (iseries-null-value iseries))
          (vector-set! data i #f)
          (vector-set! data i #t)))
    (new-BSeries data)))
