#lang typed/racket

(require math/statistics)

(require
 (only-in "indexed-series.rkt"
	  RFIndex RFIndex? build-index-from-list
          IndexDataType extract-index
          Label LabelIndex-index
          LabelIndex label-index label->lst-idx key->lst-idx
          idx->key is-indexed? ListofIndexDataType? ListofIndex?
          ListofListofString ListofListofString? RFNULL)
 (only-in "../load/types.rkt"
          ListofString?))


; ***********************************************************
; Provide functions in this file to other files.
(provide
 (struct-out GenSeries)
 GenSeries-index
 GenericType
 GenericType?
 ListofGenericType?)

(define-type GenericType Any)

(define-predicate GenericType? GenericType)
(define-predicate ListofGenericType? (Listof GenericType))

(provide:
 [new-GenSeries ((Vectorof GenericType) (Option (U (Listof IndexDataType) RFIndex)) [#:fill-null RFNULL] -> GenSeries)]
 [set-GenSeries-index (GenSeries (U (Listof IndexDataType) RFIndex) -> GenSeries)]
 [gen-series-iref (GenSeries (Listof Index) -> GenericType)]
 [gen-series-index-ref (GenSeries IndexDataType -> (Listof GenericType))]
 [gen-series-label-ref (GenSeries Label -> GenericType)]
 [gen-series-range (GenSeries Index Index -> (Vectorof GenericType))]
 [gen-series-length (GenSeries -> Index)]
 [gen-series-referencer (GenSeries -> (Index -> GenericType))]
 [gen-series-data (GenSeries -> (Vectorof GenericType))]
 [gen-series-index (GenSeries -> (U False RFIndex))]
 [gen-series-loc-boolean (GenSeries (Listof Boolean) -> (U GenericType GenSeries))]
 [gen-series-loc (GenSeries (U Label (Listof Label) (Listof Boolean)) -> (U GenericType GenSeries))]
 [gen-series-loc-multi-index (GenSeries (U (Listof String) ListofListofString) -> (U GenericType GenSeries))]
 [gen-series-iloc (GenSeries (U Index (Listof Index)) -> (U GenericType GenSeries))]
 [gen-series-iloc-range (GenSeries Index Index -> GenSeries)]
 [map/gen-s (GenSeries (GenericType -> GenericType) -> GenSeries)]
 [gen-series-print (GenSeries Output-Port -> Void)])

; ***********************************************************

(struct GenSeries
  ([index : (Option RFIndex)]
   [data : (Vectorof GenericType)]
   [null-value : RFNULL])
  #:mutable
  #:transparent)

; Consumes a Vector of Fixnum and a list of Labels which
; can come in list form or SIndex form and produces a GenSeries
; struct object.
(: new-GenSeries ((Vectorof GenericType) (Option (U (Listof IndexDataType) RFIndex)) [#:fill-null RFNULL] -> GenSeries))
(define (new-GenSeries data labels #:fill-null [null-value 'NA])

  (: check-mismatch (RFIndex -> Void))
  (define (check-mismatch index)    
    (let ((index-length (apply + (for/list: : (Listof Index)
                                   ([value (in-hash-values (extract-index index))])
                                   (length (assert value ListofIndex?))))))

      (unless (eq? (vector-length data) index-length)
        (let ((k (current-continuation-marks)))
          (raise (make-exn:fail:contract "Cardinality of a Series' data and labels must be equal" k))))
      (void)))

  (if (RFIndex? labels)
      (begin
	(check-mismatch labels)
	(GenSeries labels data null-value))
      (if labels
	  (let ((index (build-index-from-list (assert labels ListofIndexDataType?))))
	    (check-mismatch index)
	    (GenSeries index data null-value))
	  (GenSeries #f data null-value))))

; ***********************************************************

; ***********************************************************
(: set-GenSeries-index (GenSeries (U (Listof IndexDataType) RFIndex) -> GenSeries))
(define (set-GenSeries-index gen-series labels)
  (new-GenSeries (gen-series-data gen-series) labels))
; ***********************************************************

; ***********************************************************
; This function consumes a generic series and returns a
; lambda function which consumes an index and provides the
; value of the data at that index in the series. It can be
; defined once and used repeatedly as a referencer.
(: gen-series-referencer (GenSeries -> (Index -> GenericType)))
(define (gen-series-referencer gen-series)
  (let ((data (GenSeries-data gen-series)))
    (λ: ((idx : Index))
	(vector-ref data idx))))

; This function consumes a generic series and an index and
; returns the value at that index in the series.
(: gen-series-iref (GenSeries (Listof Index) -> (Listof GenericType)))
(define (gen-series-iref series lst-idx)
  (map (lambda ((idx : Index)) (vector-ref (GenSeries-data series) idx))
       lst-idx))

; This function consumes a series and a Label and returns
; the list of values at that Label in the series.
(: gen-series-index-ref (GenSeries IndexDataType -> (Listof GenericType)))
(define (gen-series-index-ref series item)
  (gen-series-iref series (key->lst-idx (assert (GenSeries-index series)) item)))

; This function consumes a generic series and an index and
; returns a vector of values in the range [0:index] in the series.
(: gen-series-range (GenSeries Index Index -> (Vectorof GenericType)))
(define (gen-series-range series start end)
   (vector-copy (GenSeries-data series) start end))

; This function consumes a generic series and returns its
; data vector.
(: gen-series-data (GenSeries -> (Vectorof GenericType)))
(define (gen-series-data series)
  (GenSeries-data series))

; This function consumes a generic series and returns its
; data vector.
(: gen-series-index (GenSeries -> (U False RFIndex)))
(define (gen-series-index series)
  (GenSeries-index series))

; This function consumes an integer series and returns its
; data vector.
(: gen-series-null-value (GenSeries -> GenericType))
(define (gen-series-null-value series)
  (GenSeries-null-value series))

; This function consumes a generic series and a Label and returns
; the value at that Label in the series.
(: gen-series-label-ref (GenSeries IndexDataType -> (Listof GenericType)))
(define (gen-series-label-ref series label)
  (gen-series-iref series (key->lst-idx (assert (GenSeries-index series)) label)))

; This function consumes a generic series and returns the
; length of that series.
(: gen-series-length (GenSeries -> Index))
(define (gen-series-length series)
  (vector-length (GenSeries-data series)))
; ***********************************************************

; ***********************************************************
(: map/gen-s (GenSeries (GenericType -> GenericType) -> GenSeries))
(define (map/gen-s series fn)
  (let ((old-data (GenSeries-data series)))
    (GenSeries #f (build-vector (vector-length old-data)
                                (λ: ((idx : Natural))
                                  (fn (vector-ref old-data idx)))) (gen-series-null-value series))))
; ***********************************************************

; ***********************************************************
; indexing

(: build-labels-by-count ((Listof Label) (Listof Integer) -> (Listof Label)))
(define (build-labels-by-count label-lst count-lst)
  (if (null? label-lst)
      null
      (append
       (for/list: : (Listof Label)
         ([i (car count-lst)])
         (car label-lst))
    
       (build-labels-by-count (cdr label-lst) (cdr count-lst)))))

(: convert-to-label-lst ((U Label (Listof Label)) -> (Listof Label)))
(define (convert-to-label-lst label)
  (if (list? label)
      label
      (list label)))

(define-predicate ListofBoolean? (Listof Boolean))

; label based
; for two different use cases:
; a.) Selecting rows by label/index
; b.) Selecting rows with a boolean / conditional lookup

; Valid inputs
; A single label, e.g. 'a'.
; A list or array of labels ['a', 'b', 'c'].
; A boolean array.

(: true? (Boolean -> Boolean))
(define (true? boolean)
  (not (false? boolean)))

(: gen-series-loc-boolean (GenSeries (Listof Boolean) -> (U GenericType GenSeries)))
(define (gen-series-loc-boolean gen-series boolean-lst)
  (: data (Vectorof GenericType))
  (define data (gen-series-data gen-series))

  (: new-data (Vectorof GenericType))
  (define new-data (make-vector (length (filter true? boolean-lst)) #f))
  
  (define data-idx 0)
  (define new-data-idx 0)

  (if (= (length boolean-lst) 1)
      (if (list-ref boolean-lst 0)
          (vector-ref data 0)
          ; empty GenSeries
          (new-GenSeries (vector) #f))
       
      (for ([b boolean-lst]
            [d data])
        (begin
          (when b
            (begin              
              (vector-set! new-data new-data-idx (vector-ref data data-idx))
              (set! new-data-idx (add1 new-data-idx))))
          (set! data-idx (add1 data-idx)))))

  (if (= (vector-length new-data) 1)
      (vector-ref new-data 0)
      (new-GenSeries new-data #f)))


(: gen-series-loc-multi-index (GenSeries (U (Listof String) ListofListofString) -> (U GenericType GenSeries)))
(define (gen-series-loc-multi-index gen-series label)
  (unless (GenSeries-index gen-series)
    (let ((k (current-continuation-marks)))
      (raise (make-exn:fail:contract "gen-series must have a label index." k))))

  (: get-index-val ((Listof String) -> Symbol))
  (define (get-index-val label)
    (string->symbol (string-append (string-join label "\t") "\t")))
  
  (if (ListofListofString? label)
      (gen-series-loc gen-series (map get-index-val label))
      (gen-series-loc gen-series (get-index-val label))))

(: gen-series-loc (GenSeries (U Label (Listof Label) (Listof Boolean)) -> (U GenericType GenSeries)))
(define (gen-series-loc gen-series label)
  (unless (GenSeries-index gen-series)
    (let ((k (current-continuation-marks)))
      (raise (make-exn:fail:contract "gen-series must have a label index." k))))

  (if (ListofBoolean? label)
      (gen-series-loc-boolean gen-series label)
      (let ((associated-indices-length : (Listof Integer)
                                       (map (lambda ([l : Label]) (length (gen-series-label-ref gen-series l))) (convert-to-label-lst label)))
            (vals : (Vectorof GenericType)
             (if (list? label)
                 (list->vector (assert (flatten (map (lambda ([l : Label]) (gen-series-label-ref gen-series l)) label)) ListofBoolean?))
                 (list->vector (assert (gen-series-label-ref gen-series label) ListofGenericType?)))))

        (if (= (vector-length vals) 1)
            (vector-ref vals 0)
            (new-GenSeries vals (build-index-from-list (build-labels-by-count (convert-to-label-lst label) associated-indices-length)))))))

; index based
(: gen-series-iloc (GenSeries (U Index (Listof Index)) -> (U GenericType GenSeries)))
(define (gen-series-iloc gen-series idx)
  (let ((referencer (gen-series-referencer gen-series)))
  (if (list? idx)
      ; get labels from SIndex that refer to given indicies
      ; make a new index from these labels using build-index-from-labels
      ; sub-vector the data vector to get the data and create a new-GenSeries
      (new-GenSeries
       (for/vector: : (Vectorof GenericType) ([i idx])
         (vector-ref (gen-series-data gen-series) i))
       (if (not (GenSeries-index gen-series))
           #f
           (build-index-from-list (map (lambda ([i : Index]) (idx->key (assert (GenSeries-index gen-series)) i)) idx))))
      (referencer idx))))

(: gen-series-iloc-range (GenSeries Index Index -> GenSeries))
(define (gen-series-iloc-range gen-series start end)
  ; use vector-copy library method
  (new-GenSeries
   (vector-copy (gen-series-data gen-series) start end)
   (if (not (GenSeries-index gen-series))
       #f
       (build-index-from-list (map (lambda ([i : Index]) (idx->key (assert (GenSeries-index gen-series)) i)) (range start end))))))

; ***********************************************************

; ***********************************************************
(: gen-series-print (GenSeries Output-Port -> Void))
(define (gen-series-print gen-series port)
  (define v (gen-series-data gen-series))
  (let ((len (vector-length v))
	(out (current-output-port)))
    (if (zero? len)
	(displayln "Empty $GenSeries" port)
	(begin
          (displayln "*********")
          (displayln "$GenSeries" port)
          (displayln "*********")
	  (do ((i 0 (add1 i)))
	      ((>= i len) (void))
	    (let ((val (vector-ref v i)))
              (if (GenSeries-index gen-series)
                  (display (idx->key (assert (GenSeries-index gen-series)) (assert i index?)) port)
                  (display (assert i index?) port))
              (display " " port)
              (displayln val port)))))))
; ***********************************************************