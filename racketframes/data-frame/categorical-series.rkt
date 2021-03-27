#lang typed/racket

(provide
 (struct-out CSeries)
 new-CSeries)
;;writer-CSeries)

(provide:
 [set-CSeries-index (CSeries (U (Listof IndexDataType) RFIndex) -> CSeries)]
 [set-CSeries-null-value (CSeries RFNULL -> CSeries)]
 [set-CSeries-label-null-value-inplace (CSeries Label -> Void)]
 [cseries-length      (CSeries -> Index)]
 [cseries-iref        (CSeries (Listof Index) -> (Listof Label))]
 [cseries-range (CSeries Index -> (Vectorof Label))]
 [cseries-data        (CSeries -> (Vectorof Symbol))]
 [cseries-index (CSeries -> (U False RFIndex))]
 [cseries-null-value (CSeries -> Label)]
 [cseries-custom-null-value (CSeries -> RFNULL)]
 [cseries-value-is-null? (CSeries Label -> Boolean)]
 [cseries-referencer (CSeries -> (Fixnum -> Label))]
 [cseries-iloc (CSeries (U Index (Listof Index)) -> (U Label CSeries))]
 [cseries-groupby (CSeries [#:by-value Boolean] -> GroupHash)]
 [cseries-index-ref (CSeries IndexDataType -> (Listof Label))]
 [cseries-print (CSeries [#:output-port Output-Port] -> Void)]
 [cseries-loc-boolean (CSeries (Listof Boolean) -> (U Label CSeries))]
 [cseries-loc (CSeries (U Label (Listof Label) (Listof Boolean)) -> (U Label CSeries))]
 [cseries-loc-multi-index (CSeries (U (Listof String) ListofListofString) -> (U Label CSeries))])

(require
 (only-in "indexed-series.rkt"
	  RFIndex RFIndex? RFNULL Label Label? idx->key key->lst-idx extract-index build-index-from-list
          build-multi-index-from-list LabelIndex LabelIndex-index is-labeled? IndexDataType
          ListofIndex? ListofListofString ListofListofString?))

(define-type CSeriesFn (Label -> Label))

;; Categorical Series
;; Encoded as an array of integer values with an associated nominal.
;; Custom Structure Writer
(: cseries-print (CSeries [#:output-port Output-Port] -> Void))
(define (cseries-print cseries #:output-port [port (current-output-port)])
  (let* ([data (CSeries-data cseries)]
	 [nominals (CSeries-nominals cseries)]
	 [len (vector-length data)])
    (if (zero? len)
	(displayln "Empty $CSeries" port)
	(begin
          (displayln "*********" port)
          (displayln "$CSeries" port)
          (displayln "*********" port)
          (do ([i 0 (add1 i)])
            ((>= i len) (void))
            (if (CSeries-index cseries)                  
                (display (idx->key (assert (cseries-index cseries)) (assert i index?)) port)
                (display (assert i index?) port))
            (display " " port)

            (let* ((val (vector-ref nominals (vector-ref data i)))
                   (display-val (if (cseries-value-is-null? cseries val) (cseries-custom-null-value cseries) val)))
              (displayln display-val)))))))

(define DEFAULT_NULL_VALUE : Label 'null)
(struct: CSeries ([index : (Option RFIndex)]
                  [data : (Vectorof Index)]
                  [nominals : (Vectorof Label)]
                  ; when the null-value is not a fixnum?, the fixnum-null-value is set to 0
                  [null-value : RFNULL]
                  ; needed for type checking purposes and to do proper arithmetic operations in numeric series
                  [label-null-value : Label])
  #:mutable
  #:transparent)

;; #:methods gen:custom-write [(define write-proc writer-CSeries)])

(: new-CSeries ((Vectorof Label) (Option (U (Listof IndexDataType) RFIndex)) [#:fill-null RFNULL]  -> CSeries))
(define (new-CSeries nominals labels #:fill-null [null-value DEFAULT_NULL_VALUE])

  (: nominal-code (HashTable Label Index))
  (define nominal-code (make-hash))

  (define label-null-value : Label (if (Label? null-value) null-value DEFAULT_NULL_VALUE))

  (define len (vector-length nominals))

  (: data (Vectorof Index))
  (define data (make-vector len 0))

  (: make-nominal-vector (-> (Vectorof Label)))
  (define (make-nominal-vector)
    (define nominals (make-vector (hash-count nominal-code) 'Null))
    (hash-for-each nominal-code
		   (λ: ((nom : Label) (idx : Index))
		       (vector-set! nominals idx nom)))
    nominals)

  (: check-mismatch (RFIndex -> Void))
  (define (check-mismatch index)    
    (let ((index-length (apply + (for/list: : (Listof Index)
                                   ([value (in-hash-values (extract-index index))])
                                   (length (assert value ListofIndex?))))))

      (unless (eq? (vector-length data) index-length)
        (let ((k (current-continuation-marks)))
          (raise (make-exn:fail:contract "Cardinality of a Series' data and labels must be equal" k))))
      (void)))
  
  (let: loop : CSeries ((idx : Natural 0) (code : Index 0))
	(if (>= idx len)
            (if (RFIndex? labels)
                (begin
                  (check-mismatch labels)
                  (CSeries labels data (make-nominal-vector) null-value label-null-value))
                (CSeries #f data (make-nominal-vector) null-value label-null-value))
	    (let ((nom (vector-ref nominals idx)))
	      (if (hash-has-key? nominal-code nom)
		  (begin
		    (vector-set! data idx (hash-ref nominal-code nom))
		    (loop (add1 idx) code))
		  (begin
		    (hash-set! nominal-code nom code)
		    (vector-set! data idx code)
		    (loop (add1 idx) (assert (add1 code) index?))))))))

; ***********************************************************
(: set-CSeries-index (CSeries (U (Listof IndexDataType) RFIndex) -> CSeries))
(define (set-CSeries-index cseries labels)
  (new-CSeries (CSeries-nominals cseries) labels))

(: set-CSeries-null-value (CSeries RFNULL -> CSeries))
(define (set-CSeries-null-value cseries null-value)
  (new-CSeries (cseries-data cseries) (cseries-index cseries) #:fill-null null-value))

(: set-CSeries-label-null-value-inplace (CSeries Label -> Void))
(define (set-CSeries-label-null-value-inplace cseries null-value)
  (set-CSeries-label-null-value! cseries null-value))
; ***********************************************************

(: cseries-referencer (CSeries -> (Fixnum -> Label)))
(define (cseries-referencer cseries)
  (let ((data (CSeries-data cseries))
	(noms (CSeries-nominals cseries)))
    (λ: ((idx : Fixnum))
	(let ((code (vector-ref data idx)))
	  (vector-ref noms code)))))

(: cseries-iref (CSeries (Listof Index) -> (Listof Label)))
(define (cseries-iref cseries lst-idx)
  (map (lambda ((idx : Index))
         (vector-ref (CSeries-nominals cseries)
                     (vector-ref (CSeries-data cseries) idx)))
       lst-idx))

; This function consumes an integer series and an index and
; returns a vector of values in the range [0:index] in the series.
(: cseries-range (CSeries Index -> (Vectorof Label)))
(define (cseries-range series pos)
  (vector-map (lambda ([idx : Index]) (vector-ref (CSeries-nominals series) idx))
              (vector-take (CSeries-data series) pos)))

(: cseries-length (CSeries -> Index))
(define (cseries-length series)
  (vector-length (CSeries-data series)))

(: cseries-data (CSeries -> (Vectorof Symbol)))
(define (cseries-data series)
  (vector-map (lambda ([i : Index]) (car (cseries-iref series (list i)))) (CSeries-data series)))

(: cseries-index (CSeries -> (U False RFIndex)))
(define (cseries-index series)
  (CSeries-index series))

; This function consumes an integer series and returns its
; data vector.
(: cseries-custom-null-value (CSeries -> RFNULL))
(define (cseries-custom-null-value cseries)
  (CSeries-null-value cseries))

; This function consumes an integer series and returns its
; data vector.
(: cseries-null-value (CSeries -> Label))
(define (cseries-null-value cseries)
  (CSeries-label-null-value cseries))

; This function consumes an integer series and a integer value
; and returns whether it is considered to be NULL in the series.
(: cseries-value-is-null? (CSeries Label -> Boolean))
(define (cseries-value-is-null? cseries value)
  (eq? (cseries-null-value cseries) value))

; label based
(: build-labels-by-count ((Listof Label) (Listof Integer) -> (Listof Label)))
(define (build-labels-by-count label-lst count-lst)
  (if (null? label-lst)
      null
      (append
       (for/list: : (Listof Label)
         ([i (car count-lst)])
         (car label-lst))
       
       (build-labels-by-count (cdr label-lst) (cdr count-lst)))))

; This function consumes a series and a Label and returns
; the list of values at that Label in the series.
(: cseries-index-ref (CSeries IndexDataType -> (Listof Label)))
(define (cseries-index-ref series item)
  (cseries-iref series (key->lst-idx (assert (CSeries-index series)) item)))

(: convert-to-label-lst ((U Label (Listof Label)) -> (Listof Label)))
(define (convert-to-label-lst label)
  (if (list? label)
      label
      (list label)))

(define-predicate ListofBoolean? (Listof Boolean))
(define-predicate ListofLabel? (Listof Label))

; label based
; for two different use cases:
; a.) Selecting rows by label/index
; b.) Selecting rows with a boolean / conditional lookup

(: cseries-loc-multi-index (CSeries (U (Listof String) ListofListofString) -> (U Label CSeries)))
(define (cseries-loc-multi-index cseries label)
  (unless (CSeries-index cseries)
    (let ((k (current-continuation-marks)))
      (raise (make-exn:fail:contract "cseries must have a label index." k))))

  (: get-index-val ((Listof String) -> Symbol))
  (define (get-index-val label)
    (string->symbol (string-append (string-join label "\t") "\t")))
  
  (if (ListofListofString? label)
      (cseries-loc cseries (map get-index-val label))
      (cseries-loc cseries (get-index-val label))))
    
    
(: cseries-loc (CSeries (U Label (Listof Label) (Listof Boolean)) -> (U Label CSeries)))
(define (cseries-loc cseries label)
  (unless (CSeries-index cseries)
    (let ((k (current-continuation-marks)))
      (raise (make-exn:fail:contract "iseries must have an index." k))))

  (if (ListofBoolean? label)
      (cseries-loc-boolean cseries label)
      (let ((associated-indices-length : (Listof Integer)
                                       (map (lambda ([l : Label]) (length (cseries-index-ref cseries l))) (convert-to-label-lst label)))
            (vals : (Vectorof Label)
             (if (list? label)
                 (list->vector (assert (flatten (map (lambda ([l : Label]) (cseries-index-ref cseries l)) label)) ListofLabel?))
                 (list->vector (assert (cseries-index-ref cseries label) ListofLabel?)))))

        (if (= (vector-length vals) 1)
            (vector-ref vals 0)
            (new-CSeries vals (build-index-from-list (build-labels-by-count (convert-to-label-lst label) associated-indices-length)))))))

; index based
(: cseries-iloc (CSeries (U Index (Listof Index)) -> (U Label CSeries)))
(define (cseries-iloc cseries idx)
  (let ((referencer (cseries-referencer cseries)))
  (if (list? idx)
      ; get labels from SIndex that refer to given indicies
      ; make a new index from these labels using build-index-from-labels
      ; sub-vector the data vector to get the data and create a new-BSeries
      (new-CSeries
       (for/vector: : (Vectorof Label) ([i idx])
         (vector-ref (cseries-data cseries) i))
       
       (if (not (CSeries-index cseries))
           (build-index-from-list (range (length idx)))
           (build-index-from-list (map (lambda ([i : Index]) (idx->key (assert (CSeries-index cseries)) i)) idx))))
      (referencer idx))))

(: true? (Boolean -> Boolean))
(define (true? boolean)
  (not (false? boolean)))

(: cseries-loc-boolean (CSeries (Listof Boolean) -> (U Label CSeries)))
(define (cseries-loc-boolean cseries boolean-lst)
  (: data (Vectorof Label))
  (define data (cseries-data cseries))

  (: new-data (Vectorof Label))
  (define new-data (make-vector (length (filter true? boolean-lst)) 'label))
  
  (define data-idx 0)
  (define new-data-idx 0)

  (if (= (length boolean-lst) 1)
      (if (list-ref boolean-lst 0)
          (vector-ref data 0)
          ; empty cseries
          (new-CSeries (vector) #f))
       
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
      (new-CSeries new-data #f)))

;; CSeries groupby

(define-type Key String)
(define-type GroupHash (HashTable Key (Listof Label)))

; This function is self-explanatory, it consumes no arguments
; and creates a hash map which will represent a JoinHash.
(: make-group-hash (-> GroupHash))
(define (make-group-hash)
  (make-hash))

; Used to determine the groups for the groupby on each value of the series index by default.
; The Series VALUES will be used to determine the groups if by-value is set to true.
(: cseries-groupby (CSeries [#:by-value Boolean] -> GroupHash))
(define (cseries-groupby cseries #:by-value [by-value #f])
  (define: group-index : GroupHash (make-group-hash))

  (let ((len (cseries-length cseries))
        (k (current-continuation-marks)))
    (if (zero? len)
	(raise (make-exn:fail:contract "cseries can't be empty on groupby." k))
	(begin          
	  (do ((i 0 (add1 i)))
	      ((>= i len) group-index)
	    (let* ((label-val : (U Label CSeries) (cseries-iloc cseries (assert i index?)))
                   (label-list : (Listof Label) (if (Label? label-val) (list label-val) (vector->list (cseries-data label-val))))
                   (key (if by-value
                            (cseries-iloc cseries (assert i index?))
                            (if (cseries-index cseries)
                                (idx->key (assert (cseries-index cseries)) (assert i index?))
                                (assert i index?))))
                  (key-str : String (cond
                                      [(symbol? key) (symbol->string key)]
                                      [(number? key) (number->string key)]
                                      ; pretty-format anything else
                                      [else (pretty-format key)])))              
              (hash-update! group-index key-str
			      (λ: ((val : (Listof Label)))                                
				  (append label-list val))
			      (λ () (list)))))))))