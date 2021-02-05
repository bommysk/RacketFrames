;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*    Author: Shubham Kahal
;*    File: boolean-series.rkt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket

; ***********************************************************
; One-dimensional array like structure with axis labels. Labels
; must be unique and must be a hashable type. The series object
; supports both integer (idx) and label-based indexing. Functions
; can be mapped to each value of the series allowing for various
; operations. The boolean series is optimized for working booleans.
; ***********************************************************

; ***********************************************************
; Provide functions in this file to other files.
(provide
 (struct-out BSeries))

(provide:
 [new-BSeries ((Vectorof Boolean) (Option (U (Listof IndexDataType) RFIndex)) [#:fill-null Boolean] -> BSeries)]
 [set-BSeries-index (BSeries (U (Listof IndexDataType) RFIndex) -> BSeries)]
 [set-BSeries-null-value (BSeries Boolean -> BSeries)]
 [bseries-iref (BSeries (Listof Index) -> (Listof Boolean))]
 [bseries-index-ref (BSeries IndexDataType -> (Listof Boolean))]
 [bseries-range (BSeries Index Index -> (Vectorof Boolean))]
 [bseries-length (BSeries -> Index)]
 [bseries-referencer (BSeries -> (Index -> Boolean))]
 [bseries-data (BSeries -> (Vectorof Boolean))]
 [bseries-index (BSeries -> (U False RFIndex))]
 [bseries-groupby (BSeries [#:by-value Boolean] -> GroupHash)]
 [map/bs (BSeries (Boolean -> Boolean) -> BSeries)]
 [bseries-loc-boolean (BSeries (Listof Boolean) -> (U Boolean BSeries))]
 [bseries-loc (BSeries (U Label (Listof Label) (Listof Boolean)) -> (U Boolean BSeries))]
 [bseries-loc-multi-index (BSeries (U (Listof String) ListofListofString) -> (U Boolean BSeries))]
 [bseries-iloc (BSeries (U Index (Listof Index)) -> (U Boolean BSeries))]
 [bseries-iloc-range (BSeries Index Index -> BSeries)]
 [bseries-not (BSeries -> BSeries)]
 [bseries-print (BSeries Output-Port -> Void)])
; ***********************************************************

; ***********************************************************
; use build-index-from-labels function and Label, SIndex and
; LabelIndex structs from indexed-series.
(require
 racket/unsafe/ops
 (only-in "indexed-series.rkt"
	  RFIndex RFIndex? build-index-from-list
          IndexDataType extract-index
          Label LabelIndex-index
          LabelIndex label-index label->lst-idx key->lst-idx
          idx->key ListofIndexDataType? ListofIndex?
          ListofListofString ListofListofString?))
; ***********************************************************

; ***********************************************************
;; Boolean series.
(struct BSeries ([index : (Option RFIndex)]
                 [data : (Vectorof Boolean)]
                 [null-value : Boolean])  
  #:mutable
  #:transparent)

; Consumes a Vector of Fixnum and a list of Labels which
; can come in list form or SIndex form and produces a ISeries
; struct object.
(: new-BSeries ((Vectorof Boolean) (Option (U (Listof IndexDataType) RFIndex)) [#:fill-null Boolean] -> BSeries))
(define (new-BSeries data labels #:fill-null [null-value #f])

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
	(BSeries labels data null-value))
      (if labels
	  (let ((index (build-index-from-list (assert labels ListofIndexDataType?))))
	    (check-mismatch index)
	    (BSeries index data null-value))
	  (BSeries #f data null-value))))
; ***********************************************************

; ***********************************************************
(: set-BSeries-index (BSeries (U (Listof IndexDataType) RFIndex) -> BSeries))
(define (set-BSeries-index bseries labels)
  (new-BSeries (bseries-data bseries) labels))

(: set-BSeries-null-value (BSeries Boolean -> BSeries))
(define (set-BSeries-null-value bseries null-value)
  (new-BSeries (bseries-data bseries) (bseries-index bseries) #:fill-null null-value))
; ***********************************************************

; ***********************************************************
; This function consumes an integer series and returns a
; lambda function which consumes an index and provides the
; value of the data at that index in the series. It can be
; defined once and used repeatedly as a referencer.
(: bseries-referencer (BSeries -> (Index -> Boolean)))
(define (bseries-referencer bseries)
  (let ((data (BSeries-data bseries)))
    (λ: ((idx : Index))
	(vector-ref data idx))))

; This function consumes an integer series and an index and
; returns the value at that index in the series.
(: bseries-iref (BSeries (Listof Index) -> (Listof Boolean)))
(define (bseries-iref series lst-idx)
  (map (lambda ((idx : Index)) (vector-ref (BSeries-data series) idx))
       lst-idx))

; This function consumes an integer series and an index and
; returns a vector of values in the range [0:index] in the series.
(: bseries-range (BSeries Index Index -> (Vectorof Boolean)))
(define (bseries-range series start end)
   (vector-copy (BSeries-data series) start end))

; This function consumes an integer series and returns its
; data vector.
(: bseries-data (BSeries -> (Vectorof Boolean)))
(define (bseries-data series)
  (BSeries-data series))

; This function consumes an integer series and returns its
; data vector.
(: bseries-index (BSeries -> (U False RFIndex)))
(define (bseries-index series)
  (BSeries-index series))

; This function consumes a series and a Label and returns
; the value at that Label in the series.
(: bseries-index-ref (BSeries IndexDataType -> (Listof Boolean)))
(define (bseries-index-ref series item)
  (bseries-iref series (key->lst-idx (assert (BSeries-index series)) item)))

; This function consumes an integer series and returns the
; length of that series.
(: bseries-length (BSeries -> Index))
(define (bseries-length series)
  (vector-length (BSeries-data series)))
; ***********************************************************

; ***********************************************************
(: map/bs (BSeries (Boolean -> Boolean) -> BSeries))
(define (map/bs series fn)
  (let ((old-data (BSeries-data series)))
    (BSeries #f (build-vector (vector-length old-data)
                              (λ: ((idx : Natural))
                                (fn (vector-ref old-data idx)))) (BSeries-null-value series))))
; ***********************************************************

; ***********************************************************
; Indexing

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

(: bseries-loc-boolean (BSeries (Listof Boolean) -> (U Boolean BSeries)))
(define (bseries-loc-boolean bseries boolean-lst)
  (: data (Vectorof Boolean))
  (define data (bseries-data bseries))

  (: new-data (Vectorof Boolean))
  (define new-data (make-vector (length (filter true? boolean-lst)) #f))
  
  (define data-idx 0)
  (define new-data-idx 0)

  (if (= (length boolean-lst) 1)
      (if (list-ref boolean-lst 0)
          (vector-ref data 0)
          ; empty BSeries
          (new-BSeries (vector) #f))
       
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
      (new-BSeries new-data #f)))

(: bseries-loc-multi-index (BSeries (U (Listof String) ListofListofString) -> (U Boolean BSeries)))
(define (bseries-loc-multi-index bseries label)
  (unless (BSeries-index bseries)
    (let ((k (current-continuation-marks)))
      (raise (make-exn:fail:contract "bseries must have a label index." k))))

  (: get-index-val ((Listof String) -> Symbol))
  (define (get-index-val label)
    (string->symbol (string-append (string-join label "\t") "\t")))
  
  (if (ListofListofString? label)
      (bseries-loc bseries (map get-index-val label))
      (bseries-loc bseries (get-index-val label))))
    
(: bseries-loc (BSeries (U Label (Listof Label) (Listof Boolean)) -> (U Boolean BSeries)))
(define (bseries-loc bseries label)
  (unless (BSeries-index bseries)
    (let ((k (current-continuation-marks)))
      (raise (make-exn:fail:contract "BSeries must have a label index." k))))

  (if (ListofBoolean? label)
      (bseries-loc-boolean bseries label)
      (let ((associated-indices-length : (Listof Integer)
                                       (map (lambda ([l : Label]) (length (bseries-index-ref bseries l))) (convert-to-label-lst label)))
            (vals : (Vectorof Boolean)
             (if (list? label)
                 (list->vector (assert (flatten (map (lambda ([l : Label]) (bseries-index-ref bseries l)) label)) ListofBoolean?))
                 (list->vector (assert (bseries-index-ref bseries label) ListofBoolean?)))))

        (if (= (vector-length vals) 1)
            (vector-ref vals 0)
            (new-BSeries vals (build-index-from-list (build-labels-by-count (convert-to-label-lst label) associated-indices-length)))))))


; index based
(: bseries-iloc (BSeries (U Index (Listof Index)) -> (U Boolean BSeries)))
(define (bseries-iloc bseries idx)
  (let ((referencer (bseries-referencer bseries)))
  (if (list? idx)
      ; get labels from RFIndex that refer to given indicies
      ; make a new index from these labels using build-index-from-labels
      ; sub-vector the data vector to get the data and create a new-BSeries
      (new-BSeries
       (for/vector: : (Vectorof Boolean) ([i idx])
         (referencer (assert i index?)))
       (if (not (BSeries-index bseries))
           #f
           (build-index-from-list (map (lambda ([i : Index]) (idx->key (assert (BSeries-index bseries)) i)) idx))))
      (referencer idx))))

(: bseries-iloc-range (BSeries Index Index -> BSeries))
(define (bseries-iloc-range bseries start end)
  ; use vector-copy library method
  (new-BSeries
   (vector-copy (bseries-data bseries) start end)
   (if (not (BSeries-index bseries))
       #f
       (build-index-from-list (map (lambda ([i : Index]) (idx->key (assert (BSeries-index bseries)) i)) (range start end))))))

; ***********************************************************

; ***********************************************************
;; BSeries groupby
(define-type Key String)
(define-type GroupHash (HashTable Key (Listof Boolean)))

; This function is self-explanatory, it consumes no arguments
; and creates a hash map which will represent a JoinHash.
(: make-group-hash (-> GroupHash))
(define (make-group-hash)
  (make-hash))

; Used to determine the groups for the groupby. If by is a function, it’s called on each value of the object’s index.
; The Series VALUES will be used to determine the groups.
(: bseries-groupby (BSeries [#:by-value Boolean] -> GroupHash))
(define (bseries-groupby bseries #:by-value [by-value #f])
  (define: group-index : GroupHash (make-group-hash))
  
  (let ((len (bseries-length bseries))
        (k (current-continuation-marks)))
    (if (zero? len)
        (raise (make-exn:fail:contract "bseries can't be empty on groupby." k))
        (begin
          (do ((i 0 (add1 i)))
            ((>= i len) group-index)
            (let* ((bool-val : (U Boolean BSeries) (bseries-iloc bseries (assert i index?)))
                   (bool-list : (Listof Boolean) (if (boolean? bool-val) (list bool-val) (vector->list (BSeries-data bool-val))))
                   (key (if by-value
                            (bseries-iloc bseries (assert i index?))
                            (if (BSeries-index bseries)
                                (idx->key (assert (BSeries-index bseries)) (assert i index?))
                                (assert i index?))))
                   (key-str : String (cond
                                       [(symbol? key) (symbol->string key)]
                                       [(number? key) (number->string key)]
                                       ; pretty-format anything else
                                       [else (pretty-format key)])))  
              (hash-update! group-index key-str
                            (λ: ((val : (Listof Boolean)))                                
                              (append bool-list val))
                            (λ () (list)))))))))

; ***********************************************************
(: bseries-print (BSeries Output-Port -> Void))
(define (bseries-print bseries port)
  (define v (bseries-data bseries))
  (let ((len (vector-length v))
	(out (current-output-port)))
    (if (zero? len)
	(displayln "Empty $BSeries" port)
	(begin
          (displayln "*********")
          (displayln "$BSeries" port)
          (displayln "*********")
	  (do ((i 0 (add1 i)))
	      ((>= i len) (void))
	    (let ((val (vector-ref v i)))
              (if (BSeries-index bseries)
                  (display (idx->key (assert (BSeries-index bseries)) (assert i index?)) port)
                  (display (assert i index?) port))
              (display " " port)
              (displayln val port)))))))
; ***********************************************************

(: bseries-not (BSeries -> BSeries))
(define (bseries-not bseries)
  (: inverted-boolean-vector (Vectorof Boolean))
  (define inverted-boolean-vector (vector-map (lambda ([b : Boolean]) (if b #f b)) (bseries-data bseries)))
  (new-BSeries inverted-boolean-vector (bseries-index bseries)))