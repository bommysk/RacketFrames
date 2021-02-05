#lang typed/racket

(require math/statistics)

; Provide functions in this file to other files.
(provide:
 [gen-series-groupby (GenSeries -> GroupHash)]
 [apply-agg-gen-series (Symbol GroupHash -> GenSeries)]
 [make-group-hash (-> GroupHash)]
 [make-agg-value-hash-sindex ((Listof (Pair String GenericType)) -> SIndex)]
 [agg-value-hash-to-gen-series (AggValueHash -> GenSeries)])

(provide GroupHash AggValueHash)
 
(require
   (only-in "indexed-series.rkt"
	  LabelIndex SIndex Label Labeling LabelProjection idx->key)
   (only-in "generic-series.rkt"
         GenSeries GenSeries? GenSeries-data GenSeries-index GenericType GenericType? gen-series-iloc gen-series-iref
         gen-series-length new-GenSeries gen-series-referencer))

(define-type Key String)
(define-type GroupHash (HashTable Key (Listof GenericType)))
(define-type AggValueHash (HashTable Key GenericType))

; This function is self-explanatory, it consumes no arguments
; and creates a hash map which will represent a JoinHash.
(: make-group-hash (-> GroupHash))
(define (make-group-hash)
  (make-hash))

; Used to determine the groups for the groupby. If by is a function, it’s called on each value of the object’s index.
; The Series VALUES will be used to determine the groups.
(: gen-series-groupby (GenSeries -> GroupHash))
(define (gen-series-groupby gen-series)
  (define: group-index : GroupHash (make-group-hash))  

  (let ((len (gen-series-length gen-series))
        (k (current-continuation-marks)))
    (if (zero? len)
	(raise (make-exn:fail:contract "genseries can't be empty on groupby." k))
	(begin          
	  (do ((i 0 (add1 i)))
	      ((>= i len) group-index)
	    (let* ((gen-val : (U GenericType GenSeries) (gen-series-iloc gen-series (assert i index?)))
                   (gen-list : (Listof GenericType) (if (GenericType? gen-val) (list gen-val) (vector->list (GenSeries-data gen-val))))
                  (key (if (GenSeries-index gen-series)
                                   (idx->key (GenSeries-index gen-series) (assert i index?))
                                   (assert i index?)))
                  (key-str : String (cond
                                      [(symbol? key) (symbol->string key)]
                                      [(number? key) (number->string key)]
                                      ; pretty-format anything else
                                      [else (pretty-format key)])))              
              (hash-update! group-index key-str
			      (λ: ((val : (Listof GenericType)))                                
				  (append gen-list val))
			      (λ () (list)))))))))

(define-predicate ListofReal? (Listof Real))

; Applies the aggregate function specificed by function-name to the values in
; the column-name column. Currently supports 3: sum, mean, count, min, max.
(: apply-agg-gen-series (Symbol GroupHash -> GenSeries))
(define (apply-agg-gen-series function-name group-hash)
  (define len (hash-count group-hash))

  (: agg-value-hash AggValueHash)
  (define agg-value-hash (make-hash))

  (hash-for-each group-hash
                 (lambda ([key : String] [val : (Listof GenericType)])
                   
                   (let ((key (assert key string?))
                         (val (assert (flatten val) ListofReal?)))
                     (hash-set! agg-value-hash key
                                (cond 
                                  [(eq? function-name 'sum) (apply + val)]
                                  [(eq? function-name 'mean) (mean val)]
                                  ;[(eq? function-name 'median) (median (vector->list (ISeries-data series)))]
                                  ;[(eq? function-name 'mode) (mode (vector->list (ISeries-data series)))]
                                  [(eq? function-name 'count) (length val)]
                                  [(eq? function-name 'min) (argmin (lambda ([x : Real]) x) val)]
                                  [(eq? function-name 'max) (argmax (lambda ([x : Real]) x) val)]
                                  [else (error 'apply-agg-data-frame "Unknown aggregate function.")])))))

  (agg-value-hash-to-gen-series agg-value-hash))

(: make-agg-value-hash-sindex ((Listof (Pair String GenericType)) -> SIndex))
(define (make-agg-value-hash-sindex sorted-agg-value-hash)
  (let ((index : SIndex (make-hash '()))
          (len (length sorted-agg-value-hash)))
    
      (begin
        (let loop ([i 0])
          (if (>= i len)
              index
              (let: ((i : Index (assert i index?)))
                (let ((key (car (list-ref sorted-agg-value-hash i))))
                  (hash-update! index (string->symbol key)
                                (λ: ((idx : (Listof Index)))
                                  (append idx (list i)))
                                (λ () (list))))
                (loop (add1 i)))))
        index)))

(: agg-value-hash-to-gen-series (AggValueHash -> GenSeries))
(define (agg-value-hash-to-gen-series agg-value-hash)
  (let ((sorted-agg-value-hash
         ((inst sort (Pair String GenericType) (Pair String GenericType))
         (hash->list agg-value-hash)
         (λ: ((kv1 : (Pair String GenericType))
              (kv2 : (Pair String GenericType)))
           (string<? (car kv1) (car kv2))))))

    (let ((index : SIndex (make-agg-value-hash-sindex sorted-agg-value-hash)))
      (new-GenSeries (for/vector: : (Vectorof GenericType) ([p sorted-agg-value-hash])
                       (cdr p)) (LabelIndex index)))))