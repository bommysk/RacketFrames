;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*    Author: Shubham Kahal
;*    File: numeric-series.rkt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket
(require math/statistics)

; ***********************************************************
; One-dimensional array like structure with axis labels. Labels
; must be unique and must be a hashable type. The series object
; supports both integer (idx) and label-based indexing. Functions
; can be mapped to each value of the series allowing for various
; operations. The integer series is optimized for working with
; integers.
; ***********************************************************

; ***********************************************************
; Provide functions in this file to other files.

(provide 
 (struct-out NSeries)
 NSeries-index (rename-out [GroupHash nseries-grouphash] [DEFAULT_NULL_VALUE NSERIES_DEFAULT_NULL_VALUE]))

(provide:
 [new-NSeries ((U (Sequenceof Flonum) FlVector) [#:index (Option (U (Sequenceof IndexDataType) RFIndex))]
                         [#:fill-null RFNULL] [#:sort Boolean] [#:encode Boolean]  -> NSeries)]
 [set-NSeries-index (NSeries (U (Listof IndexDataType) RFIndex) -> NSeries)]
 [set-NSeries-null-value (NSeries RFNULL -> NSeries)]
 [nseries-custom-null-value (NSeries -> RFNULL)]
 [nseries-null-value (NSeries -> Flonum)]
 [set-NSeries-flonum-null-value-inplace (NSeries Flonum -> Void)]
 [nseries-iref (NSeries (Listof Index) -> (Listof Flonum))]
 [nseries-loc-boolean (NSeries (Listof Boolean) -> (U Flonum NSeries))] 
 [nseries-loc (NSeries (U Label (Listof Label) (Listof Boolean)) -> (U Flonum NSeries))]
 [nseries-loc-multi-index (NSeries (U (Listof String) ListofListofString) -> (U Flonum NSeries))]
 [nseries-iloc (NSeries (U Index (Listof Index)) -> (U Flonum NSeries))]
 [nseries-iloc-range (NSeries Index Index -> NSeries)]
 [nseries-index-ref (NSeries IndexDataType -> (Listof Flonum))]
 [nseries-range (NSeries Index -> FlVector)]
 [nseries-referencer (NSeries -> (Index -> Flonum))]
 [nseries-length (NSeries -> Index)]
 [nseries-data (NSeries -> FlVector)]
 [in-nseries (Flonum NSeries -> Boolean)]
 [nseries-groupby (NSeries [#:by-value Boolean] -> GroupHash)]
 [apply-agg-nseries (Symbol GroupHash -> GenSeries)]
 [nseries-index (NSeries -> (U False RFIndex))]
 [nseries-index-from-predicate (NSeries (Flonum -> Boolean) -> RFIndex)]
 [nseries-index-from-predicate-not (NSeries (Flonum -> Boolean) -> RFIndex)]
 [nseries-data-idxes-from-predicate (NSeries (Flonum -> Boolean) -> (Listof Index))]
 [nseries-data-idxes-from-predicate-not (NSeries (Flonum -> Boolean) -> (Listof Index))]
 [nseries-filter (NSeries (Flonum -> Boolean) -> NSeries)]
 [nseries-filter-not (NSeries (Flonum -> Boolean) -> NSeries)]
 [map/ns (NSeries (Flonum -> Flonum) -> NSeries)]
 [bop/ns (NSeries NSeries (Flonum Flonum -> Flonum) -> NSeries)]
 [+/ns (NSeries NSeries -> NSeries)]
 [-/ns (NSeries NSeries -> NSeries)]
 [*/ns (NSeries NSeries -> NSeries)]
 [//ns (NSeries NSeries -> NSeries)]
 [>/ns (NSeries NSeries -> BSeries)]
 [</ns (NSeries NSeries -> BSeries)]
 [>=/ns (NSeries NSeries -> BSeries)]
 [<=/ns (NSeries NSeries -> BSeries)]
 [=/ns (NSeries NSeries -> BSeries)]
 [!=/ns (NSeries NSeries -> BSeries)]
 [+./ns (NSeries Flonum -> NSeries)]
 [-./ns (NSeries Flonum -> NSeries)]
 [*./ns (NSeries Flonum -> NSeries)]
 [/./ns (NSeries Flonum -> NSeries)]
 [+/ns/is (NSeries ISeries -> NSeries)]
 [-/ns/is (NSeries ISeries -> NSeries)]
 [*/ns/is (NSeries ISeries -> NSeries)]
 [//ns/is (NSeries ISeries -> NSeries)]
 [+/is/ns (ISeries NSeries -> NSeries)]
 [-/is/ns (ISeries NSeries -> NSeries)]
 [*/is/ns (ISeries NSeries -> NSeries)]
 [//is/ns (ISeries NSeries -> NSeries)]
 [>/ns/is (NSeries ISeries -> BSeries)]
 [</ns/is (NSeries ISeries -> BSeries)]
 [>=/ns/is (NSeries ISeries -> BSeries)]
 [<=/ns/is (NSeries ISeries -> BSeries)]
 [=/ns/is (NSeries ISeries -> BSeries)]
 [!=/ns/is (NSeries ISeries -> BSeries)]
 [>/is/ns (ISeries NSeries -> BSeries)]
 [</is/ns (ISeries NSeries -> BSeries)]
 [>=/is/ns (ISeries NSeries -> BSeries)]
 [<=/is/ns (ISeries NSeries -> BSeries)]
 [=/is/ns (ISeries NSeries -> BSeries)]
 [!=/is/ns (ISeries NSeries -> BSeries)]
 [apply-agg-ns (Symbol NSeries -> GenericType)]
 [apply-stat-ns (Symbol NSeries -> Real)]
 [flvector->list (FlVector [#:index Fixnum] -> (Listof Flonum))]
 [flvector->vector (FlVector [#:index Fixnum] -> (Vectorof Flonum))]
 [list->flvector ((Listof Flonum) -> FlVector)]
 [nseries-print (NSeries [#:output-port Output-Port] -> Void)])

; ***********************************************************

; ***********************************************************

(require
 racket/unsafe/ops
 racket/flonum
 "../util/data-encode.rkt"  
 (only-in "settings.rkt"
	  Settings-decimals
	  Settings-max-output
	  settings)
 (only-in "indexed-series.rkt"
	  RFIndex? RFNULL label-index label->lst-idx
	  build-index-from-list IndexDataType
	  SIndex Label RFIndex extract-index
	  LabelIndex LabelIndex-index
          idx->key is-indexed?
          key->lst-idx
          is-labeled? ListofIndexDataType? ListofIndex?
          ListofListofString ListofListofString?)
 (only-in "boolean-series.rkt"
          new-BSeries BSeries BSeries-data)
 (only-in "integer-series.rkt"
          ISeries ISeries-data)
 (only-in "generic-series.rkt"
	  GenSeries GenSeries? GenericType gen-series-iref new-GenSeries
	  gen-series-referencer)
 (only-in "groupby-util.rkt"
          make-agg-value-hash-sindex agg-value-hash-to-gen-series AggValueHash))

; ***********************************************************

; ***********************************************************
(: nseries-print (NSeries [#:output-port Output-Port] -> Void))
(define (nseries-print nseries #:output-port [port (current-output-port)])
  (define flv (nseries-data nseries))
  (let ((len (flvector-length flv))
	(out (current-output-port))
	(decs (Settings-decimals (settings)))
	(max-output (Settings-max-output (settings))))
    (if (zero? len)
	(displayln "Empty NSeries{}" port)
	(begin
          (displayln "*********")
          (displayln "$NSeries" port)
          (displayln "*********")
	  (do ((i 0 (add1 i)))
	      ((>= i len) (void))
	    (let ((num (flvector-ref flv i)))
              (if (NSeries-index nseries)
                  (display (idx->key (assert (NSeries-index nseries)) (assert i index?)) port)
                  (display (assert i index?) port))
              (display " " port)
	      (if (eqv? num +nan.0)
		  (displayln num port)
		  (displayln (real->decimal-string num decs) port))))))))

; ***********************************************************

; ***********************************************************
(: writer-NSeries (NSeries Output-Port Boolean -> Void))
(define (writer-NSeries series port mode)
  (let* ([data (NSeries-data series)]
	 [len (flvector-length data)])
    (displayln (format "(NSeries #:length ~s)" len) port)))

; ***********************************************************

; ***********************************************************

(struct: Summary ([mean : Flonum]
                  [variance : Flonum]
                  [min : Flonum]
                  [max : Flonum]
                  [count : Natural]
                  [nans : Natural]))

(define DEFAULT_NULL_VALUE : Flonum +nan.0)

;; An NSeries is an optimized Series for computation over vectors of Flonum
;; i.e., NSeries should be faster then (Series Flonum)
(struct: NSeries ([index : (Option RFIndex)]
                  [data : FlVector]
                  ; when the null-value is not a fixnum?, the fixnum-null-value is set to 0
                  [null-value : RFNULL]
                  ; needed for type checking purposes and to do proper arithmetic operations in numeric series
                  [flonum-null-value : Flonum]
                  ; encode data by element count to optimze memory storage and read/write operations
                  ; when data vector lacks variety
                  [encoded : Boolean]
                  [data-count-encoded : (Option (Listof (Pairof Any Real)))])
  #:mutable
  #:transparent)

; When working with very large values that occur frequently,
; it can be more performant to use the ISeries-Nominals form
; which will only keep one copy of the Fixnum and maintain a
; a light weight vector of index to reference to the nominals.
; categorical series are constructed in nominal form by default,
; but with other series types it is not
(struct: NSeries-Nominals
  ([index : (Option RFIndex)]
   [data : (Vectorof Index)]
   [nominals : FlVector]
   [null-value : RFNULL])
  #:mutable
  #:transparent)

(: seq->flvector ((Sequenceof Flonum) -> FlVector))
(define (seq->flvector seq)
  (let ((vec : FlVector (list->flvector (sequence->list seq))))
    vec))

(: new-NSeries ((U (Sequenceof Flonum) FlVector) [#:index (Option (U (Sequenceof IndexDataType) RFIndex))]
                         [#:fill-null RFNULL] [#:sort Boolean] [#:encode Boolean]  -> NSeries))
(define (new-NSeries data #:index [labels #f] #:fill-null [null-value DEFAULT_NULL_VALUE] #:sort [do-sort #f] #:encode [encode #f])
  (define data-flvector : FlVector (if (flvector? data) data (seq->flvector data)))

  (: check-mismatch (RFIndex -> Void))
  (define (check-mismatch index)    
    (let ((index-length (apply + (for/list: : (Listof Index)
                                   ([value (in-hash-values (extract-index index))])
                                   (length (assert value ListofIndex?))))))

      (unless (eq? (flvector-length data-flvector) index-length)
        (let ((k (current-continuation-marks)))
          (raise (make-exn:fail:contract "Cardinality of a Series' data and labels must be equal" k))))
      (void)))


  ; encode data by element count to avoid repetition if user elects to sort or encode series
  (let*: ((data-count-encoded : (Option (Listof (Pairof Any Real))) (if (or do-sort encode) (most-frequent-element-list data) #f))
          (data-vector : FlVector
                       (if (not data-count-encoded)
                           data-flvector
                           (assert (list->vector (most-frequent-elements data-count-encoded)) flvector?)))
          (flonum-null-value : Flonum (if (flonum? null-value) null-value DEFAULT_NULL_VALUE)))
    
    (if (RFIndex? labels)      
        (NSeries labels data-vector null-value flonum-null-value encode data-count-encoded)
        (if labels
            (let ((index (build-index-from-list (assert labels ListofIndexDataType?))))
            (check-mismatch index)
              (NSeries index data-vector null-value flonum-null-value encode data-count-encoded))
            (NSeries #f data-vector null-value flonum-null-value encode data-count-encoded)))))

; ***********************************************************

; ***********************************************************
(: set-NSeries-index (NSeries (U (Listof IndexDataType) RFIndex) -> NSeries))
(define (set-NSeries-index nseries labels)
  (new-NSeries (nseries-data nseries) #:index labels))

(: set-NSeries-null-value (NSeries RFNULL -> NSeries))
(define (set-NSeries-null-value nseries null-value)
  (new-NSeries (nseries-data nseries) #:index (nseries-index nseries) #:fill-null null-value))

(: set-NSeries-flonum-null-value-inplace (NSeries Flonum -> Void))
(define (set-NSeries-flonum-null-value-inplace nseries null-value)
  (set-NSeries-flonum-null-value! nseries null-value))
; ***********************************************************

; ***********************************************************

; This function consumes an integer series and returns a
; lambda function which consumes an index and provides the
; value of the data at that index in the series. It can be
; defined once and used repeatedly as a referencer.
(: nseries-referencer (NSeries -> (Index -> Flonum)))
(define (nseries-referencer series)
  (let ((data (NSeries-data series)))
    (λ: ((idx : Index))
	(flvector-ref data idx))))

; This function consumes an integer series and an index and
; returns the value at that index in the series.
(: nseries-iref (NSeries (Listof Index) -> (Listof Flonum)))
(define (nseries-iref series lst-idx)
  (map (lambda ((idx : Index)) (flvector-ref (NSeries-data series) idx))
       lst-idx))

; This function consumes an integer series and an index and
; returns a vector of values in the range [0:index] in the series.
; No flvector-take function available, so for loop was used.
(: nseries-range (NSeries Index -> FlVector))
(define (nseries-range series pos)
  (define flvector-ranged (make-flvector pos))

  (for [(idx (range pos))]
    (flvector-set! flvector-ranged idx (flvector-ref (NSeries-data series) idx)))
  
  flvector-ranged)

(: nseries-index-ref (NSeries IndexDataType -> (Listof Flonum)))
(define (nseries-index-ref series key)
  (nseries-iref series (key->lst-idx (assert (NSeries-index series)) key)))

; This function consumes a numeric series and returns its
; data vector.
(: nseries-data (NSeries -> FlVector))
(define (nseries-data series)
  (NSeries-data series))

; This function consumes a numeric series and returns its
; index.
(: nseries-index (NSeries -> (U False RFIndex)))
(define (nseries-index series)
  (NSeries-index series))

; This function consumes an integer series and returns its
; data vector.
(: nseries-custom-null-value (NSeries -> RFNULL))
(define (nseries-custom-null-value series)
  (NSeries-null-value series))

; This function consumes an integer series and returns its
; data vector.
(: nseries-null-value (NSeries -> Flonum))
(define (nseries-null-value series)
  (NSeries-flonum-null-value series))

; This function consumes an flonum series and a integer value
; and returns whether it is considered to be NULL in the series.
(: nseries-value-is-null? (NSeries Flonum -> Boolean))
(define (nseries-value-is-null? series value)
  (or (nan? value) (eq? (nseries-null-value series) value)))

(: nseries-length (NSeries -> Index))
(define (nseries-length nseries)
  (flvector-length (NSeries-data nseries)))

(: flvector->list (FlVector [#:index Fixnum] -> (Listof Flonum)))
(define (flvector->list flvec #:index [idx 0])
  (cond
    [(= idx (flvector-length flvec)) null]
    [else (cons (flvector-ref flvec idx) (flvector->list flvec #:index (unsafe-fx+ idx 1)))]))

(: list->flvector ((Listof Flonum) -> FlVector))
(define (list->flvector flonum-list)
  (define len : Index (length flonum-list))

  (define result-flvector (make-flvector len))

  (for([flo flonum-list]
     [i (in-range len)])
    (flvector-set! result-flvector i flo))

  result-flvector)

(: flvector->vector (FlVector [#:index Fixnum] -> (Vectorof Flonum)))
(define (flvector->vector flvec #:index [idx 0])
  (list->vector (flvector->list flvec #:index idx)))

(: in-nseries (Flonum NSeries -> Boolean))
(define (in-nseries val series)
  (if (vector-memq val (flvector->vector (nseries-data series))) #t #f))

; ***********************************************************

; ***********************************************************
;; Map a function

; This function consumes a series and a function both of Flonum
; types and applies the function to each member of the series
; returning a new series.
(: map/ns (NSeries (Flonum -> Flonum) -> NSeries))
(define (map/ns series fn)
  (let ((old-data (NSeries-data series))
	(new-data (make-flvector (flvector-length (NSeries-data series)))))
    (let ((len (flvector-length old-data)))
      (let loop ((idx 0))
	(if (< idx len)
	    (begin
	      (flvector-set! new-data idx (fn (flvector-ref old-data idx)))
	      (loop (add1 idx)))
	    (void))))
    (new-NSeries new-data #:index (nseries-index series) #:fill-null (nseries-null-value series))))

; ***********************************************************

; ***********************************************************

;; Binary NSeries Ops

(: bop/ns (NSeries NSeries (Flonum Flonum -> Flonum) -> NSeries))
(define (bop/ns ns1 ns2 bop)
  (define v1 (NSeries-data ns1))
  (define v2 (NSeries-data ns2))
  (define: len : Index (flvector-length v1))
  
  (unless (eqv? len (flvector-length v2))
	  (error '+/ns "Series must be of equal length."))
  
  (define: v-bop : FlVector (make-flvector len))

  (do: : NSeries ([idx : Fixnum 0 (unsafe-fx+ idx 1)])
       ((= idx len) (new-NSeries v-bop))
       (flvector-set! v-bop idx (bop (flvector-ref v1 idx)
				 (flvector-ref v2 idx)))))

(: +/ns (NSeries NSeries -> NSeries))
(define (+/ns ns1 ns2)
  (bop/ns ns1 ns2 fl+))

(: -/ns (NSeries NSeries -> NSeries))
(define (-/ns ns1 ns2)
  (bop/ns ns1 ns2 fl-))

(: */ns (NSeries NSeries -> NSeries))
(define (*/ns ns1 ns2)
  (bop/ns ns1 ns2 fl*))

(: //ns (NSeries NSeries -> NSeries))
(define (//ns ns1 ns2)
  (bop/ns ns1 ns2 fl/))

; ***********************************************************

; ***********************************************************

;; Binary NSeries ISeries Ops

(: bop/ns/is (NSeries ISeries (Flonum Flonum -> Flonum) -> NSeries))
(define (bop/ns/is ns1 ns2 bop)
  (define v1 (NSeries-data ns1))
  (define v2 (ISeries-data ns2))
  (define: len : Index (flvector-length v1))
  
  (unless (eqv? len (vector-length v2))
	  (error 'bop/ns "Series must be of equal length."))
  
  (define: v-bop : FlVector (make-flvector len))

  (do: : NSeries ([idx : Fixnum 0 (unsafe-fx+ idx 1)])
       ((= idx len) (new-NSeries v-bop))
       (flvector-set! v-bop idx (bop (flvector-ref v1 idx)
				 (exact->inexact (assert (vector-ref v2 idx) fixnum?))))))

; caller functions
(: +/ns/is (NSeries ISeries -> NSeries))
(define (+/ns/is ns is)
  (bop/ns/is ns is fl+))

(: -/ns/is (NSeries ISeries -> NSeries))
(define (-/ns/is ns is)
  (bop/ns/is ns is fl-))

(: */ns/is (NSeries ISeries -> NSeries))
(define (*/ns/is ns is)
  (bop/ns/is ns is fl*))

(: //ns/is (NSeries ISeries -> NSeries))
(define (//ns/is ns is)
  (bop/ns/is ns is fl/))
; ***********************************************************

; ***********************************************************

;; Binary ISeries NSeries Ops

(: bop/is/ns (ISeries NSeries (Flonum Flonum -> Flonum) -> NSeries))
(define (bop/is/ns ns1 ns2 bop)
  (define v1 (ISeries-data ns1))
  (define v2 (NSeries-data ns2))

  (define: len : Index (vector-length v1))
  
  (unless (eqv? len (flvector-length v2))
	  (error 'bop/ns "Series must be of equal length."))
  
  (define: v-bop : FlVector (make-flvector len))

  (do: : NSeries ([idx : Fixnum 0 (unsafe-fx+ idx 1)])
       ((= idx len) (new-NSeries v-bop))
       (flvector-set! v-bop idx (bop (exact->inexact (assert (vector-ref v1 idx) fixnum?))
				 (flvector-ref v2 idx)))))

; caller functions
(: +/is/ns (ISeries NSeries -> NSeries))
(define (+/is/ns is ns)
  (bop/is/ns is ns fl+))

(: -/is/ns (ISeries NSeries -> NSeries))
(define (-/is/ns is ns)
  (bop/is/ns is ns fl-))

(: */is/ns (ISeries NSeries -> NSeries))
(define (*/is/ns is ns)
  (bop/is/ns is ns fl*))

(: //is/ns (ISeries NSeries -> NSeries))
(define (//is/ns is ns)
  (bop/is/ns is ns fl/))

; ***********************************************************

; ***********************************************************
;; Scalar NSeries Ops

(: bop./ns (Flonum NSeries (Flonum Flonum -> Flonum) -> NSeries))
(define (bop./ns fl ns bop)
  (define v1 (NSeries-data ns))
  (define: len : Index (flvector-length v1))
  (define: v-bop : FlVector (make-flvector len))

  (do: : NSeries ([idx : Fixnum 0 (unsafe-fx+ idx 1)])
       ((= idx len) (new-NSeries v-bop))
       (flvector-set! v-bop idx (bop (flvector-ref v1 idx) fl))))

(: +./ns (NSeries Flonum -> NSeries))
(define (+./ns ns fl)
  (bop./ns fl ns fl+))

(: -./ns (NSeries Flonum -> NSeries))
(define (-./ns ns fl )
  (bop./ns fl ns fl-))

(: *./ns (NSeries Flonum -> NSeries))
(define (*./ns ns fl)
  (bop./ns fl ns fl*))

(: /./ns (NSeries Flonum -> NSeries))
(define (/./ns ns fl)
  (bop./ns fl ns fl/))

; ***********************************************************

; ***********************************************************
;; Binary NSeries comp

; This function consumes 2 integer series and a function which
; consumes 2 Fixnum's and produces a Boolean result for comparison.
; This function is applied to each value in the 2 series at the same
; index resulting in a new boolean point and at the end of the loop
; a new data vector. This data vector is the data of the new ISeries
; which is returned.
(: comp/ns (NSeries NSeries (Flonum Flonum -> Boolean) -> BSeries))
(define (comp/ns ns1 ns2 comp)
  (define v1 (nseries-data ns1))
  (define v2 (nseries-data ns2))
  (define: len : Index (flvector-length v1))
  
  (unless (eqv? len (flvector-length v2))
	  (error 'comp/ns "Series must be of equal length."))
  
  (define: v-comp : (Vectorof Boolean) (make-vector len #f))

  ; Do loop returns ISeries, idx to 0 and increments by 1 Fixnum on
  ; each iteration (this is the step-exprs). When the loop has gone
  ; through the whole vector, the resulting new ISeries is returned
  ; which the v-bop as the data.
  (do: : BSeries ([idx : Fixnum 0 (unsafe-fx+ idx #{1 : Fixnum})])
       ((= idx len) (new-BSeries v-comp))
       (vector-set! v-comp idx (comp (flvector-ref v1 idx)
				   (flvector-ref v2 idx)))))

; ***********************************************************

; ***********************************************************
; These functions apply various comparison operators using
; unsafe-fx and the bop./is function defined above.

(: >/ns (NSeries NSeries -> BSeries))
(define (>/ns ns1 ns2)
  (comp/ns ns1 ns2 unsafe-fl>))

(: </ns (NSeries NSeries -> BSeries))
(define (</ns ns1 ns2)
  (comp/ns ns1 ns2 unsafe-fl<))

(: >=/ns (NSeries NSeries -> BSeries))
(define (>=/ns ns1 ns2)
  (comp/ns ns1 ns2 unsafe-fl>=))

(: <=/ns (NSeries NSeries -> BSeries))
(define (<=/ns ns1 ns2)
  (comp/ns ns1 ns2 unsafe-fl<=))

(: =/ns (NSeries NSeries -> BSeries))
(define (=/ns ns1 ns2)
  (comp/ns ns1 ns2 unsafe-fl=))

(: !=/ns (NSeries NSeries -> BSeries))
(define (!=/ns ns1 ns2)
  (comp/ns ns1 ns2 (lambda ([a : Flonum] [b : Flonum]) (not (unsafe-fl= a b)))))

; ***********************************************************


; ***********************************************************
;; Binary NSeries comp

; This function consumes 2 integer series and a function which
; consumes 2 Fixnum's and produces a Boolean result for comparison.
; This function is applied to each value in the 2 series at the same
; index resulting in a new boolean point and at the end of the loop
; a new data vector. This data vector is the data of the new ISeries
; which is returned.
(: comp/ns/is (NSeries ISeries (Flonum Flonum -> Boolean) -> BSeries))
(define (comp/ns/is ns is comp)
  (define v1 (NSeries-data ns))
  (define v2 (ISeries-data is))
  (define: len : Index (flvector-length v1))
  
  (unless (eqv? len (vector-length v2))
	  (error 'comp/ns/is "Series must be of equal length."))
  
  (define: v-comp : (Vectorof Boolean) (make-vector len #f))

  ; Do loop returns ISeries, idx to 0 and increments by 1 Fixnum on
  ; each iteration (this is the step-exprs). When the loop has gone
  ; through the whole vector, the resulting new ISeries is returned
  ; which the v-bop as the data.
  (do: : BSeries ([idx : Fixnum 0 (unsafe-fx+ idx #{1 : Fixnum})])
       ((= idx len) (new-BSeries v-comp))
       (vector-set! v-comp idx (comp (flvector-ref v1 idx)
				   (exact->inexact (assert (vector-ref v2 idx) fixnum?))))))

; ***********************************************************

; ***********************************************************
; These functions apply various comparison operators using
; unsafe-fx and the bop./is function defined above.

(: >/ns/is (NSeries ISeries -> BSeries))
(define (>/ns/is ns is)
  (comp/ns/is ns is unsafe-fl>))

(: </ns/is (NSeries ISeries -> BSeries))
(define (</ns/is ns is)
  (comp/ns/is ns is unsafe-fl<))

(: >=/ns/is (NSeries ISeries -> BSeries))
(define (>=/ns/is ns is)
  (comp/ns/is ns is unsafe-fl>=))

(: <=/ns/is (NSeries ISeries -> BSeries))
(define (<=/ns/is ns is)
  (comp/ns/is ns is unsafe-fl<=))

(: =/ns/is (NSeries ISeries -> BSeries))
(define (=/ns/is ns is)
  (comp/ns/is ns is unsafe-fl=))

(: !=/ns/is (NSeries ISeries -> BSeries))
(define (!=/ns/is ns is)
  (comp/ns/is ns is (lambda ([a : Flonum] [b : Flonum]) (not (unsafe-fl= a b)))))

; ***********************************************************

; ***********************************************************
;; Binary NSeries comp

; This function consumes 2 integer series and a function which
; consumes 2 Fixnum's and produces a Boolean result for comparison.
; This function is applied to each value in the 2 series at the same
; index resulting in a new boolean point and at the end of the loop
; a new data vector. This data vector is the data of the new ISeries
; which is returned.
(: comp/is/ns (ISeries NSeries (Flonum Flonum -> Boolean) -> BSeries))
(define (comp/is/ns is ns comp)
  (define v1 (ISeries-data is))
  (define v2 (NSeries-data ns))
  (define: len : Index (vector-length v1))
  
  (unless (eqv? len (flvector-length v2))
	  (error 'comp/is/ns "Series must be of equal length."))
  
  (define: v-comp : (Vectorof Boolean) (make-vector len #f))

  ; Do loop returns ISeries, idx to 0 and increments by 1 Fixnum on
  ; each iteration (this is the step-exprs). When the loop has gone
  ; through the whole vector, the resulting new ISeries is returned
  ; which the v-bop as the data.
  (do: : BSeries ([idx : Fixnum 0 (unsafe-fx+ idx #{1 : Fixnum})])
       ((= idx len) (new-BSeries v-comp))
       (vector-set! v-comp idx (comp (exact->inexact (assert (vector-ref v1 idx) fixnum?))
				   (flvector-ref v2 idx)))))

; ***********************************************************

; ***********************************************************
; These functions apply various comparison operators using
; unsafe-fx and the bop./is function defined above.

(: >/is/ns (ISeries NSeries -> BSeries))
(define (>/is/ns is ns)
  (comp/is/ns is ns unsafe-fl>))

(: </is/ns (ISeries NSeries -> BSeries))
(define (</is/ns is ns)
  (comp/is/ns is ns unsafe-fl<))

(: >=/is/ns (ISeries NSeries -> BSeries))
(define (>=/is/ns is ns)
  (comp/is/ns is ns unsafe-fl>=))

(: <=/is/ns (ISeries NSeries -> BSeries))
(define (<=/is/ns is ns)
  (comp/is/ns is ns unsafe-fl<=))

(: =/is/ns (ISeries NSeries -> BSeries))
(define (=/is/ns is ns)
  (comp/is/ns is ns unsafe-fl=))

(: !=/is/ns (ISeries NSeries -> BSeries))
(define (!=/is/ns is ns)
  (comp/is/ns is ns (lambda ([a : Flonum] [b : Flonum]) (not (unsafe-fl= a b)))))

; ***********************************************************

; ***********************************************************
;; NSeries agg ops

; Applies the aggregate function specificed by function-name to the values in
; the column-name column. Currently supports 3: sum, avg, count.
(: apply-agg-ns (Symbol NSeries -> GenericType))
(define (apply-agg-ns function-name series)
  (cond 
    [(eq? function-name 'sum) (apply + (flvector->list (nseries-data series)))]
    [(eq? function-name 'mean) (mean (flvector->list (nseries-data series)))]
    [(eq? function-name 'median) (median < (flvector->list (nseries-data series)))]
    [(eq? function-name 'mode) (most-frequent-element (flvector->list (nseries-data series)))]
    [(eq? function-name 'count) (nseries-length series)]    
    [(eq? function-name 'min) (argmin (lambda ([x : Real]) x) (flvector->list (nseries-data series)))]
    [(eq? function-name 'max) (argmax (lambda ([x : Real]) x) (flvector->list (nseries-data series)))]
    [else (error 'apply-agg-ns "Unknown aggregate function.")]))

; ***********************************************************

; ***********************************************************
;; NSeries stat ops

(: apply-stat-ns (Symbol NSeries -> Real))
(define (apply-stat-ns function-name series)
  (cond 
    [(eq? function-name 'variance) (variance (flvector->list (nseries-data series)))]
    [(eq? function-name 'stddev) (stddev (flvector->list (nseries-data series)))]
    [(eq? function-name 'skewness) (skewness (flvector->list (nseries-data series)))]
    [else (error 'apply-stat-ns "Unknown stat function.")]))

; ***********************************************************

; ***********************************************************

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

(: convert-to-label-lst ((U Label (Listof Label)) -> (Listof Label)))
(define (convert-to-label-lst label)
  (if (list? label)
      label
      (list label)))

(define-predicate ListofBoolean? (Listof Boolean))
(define-predicate ListofFlonum? (Listof Flonum))

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

(: nseries-loc-boolean (NSeries (Listof Boolean) -> (U Flonum NSeries)))
(define (nseries-loc-boolean nseries boolean-lst)
  (: data FlVector)
  (define data (nseries-data nseries))

  (: new-data FlVector)
  (define new-data (make-flvector (length (filter true? boolean-lst)) 0.0))
  
  (define data-idx 0)
  (define new-data-idx 0)

  (if (= (length boolean-lst) 1)
      (if (list-ref boolean-lst 0)
          (flvector-ref data 0)
          ; empty nseries
          (new-NSeries (flvector)))
       
      (for ([b boolean-lst]
            [d (flvector->list data)])
        (begin
          (when b
            (begin              
              (flvector-set! new-data new-data-idx (flvector-ref data data-idx))
              (set! new-data-idx (add1 new-data-idx))))
          (set! data-idx (add1 data-idx)))))

  (if (= (flvector-length new-data) 1)
      (flvector-ref new-data 0)
      (new-NSeries new-data)))

(: nseries-loc-multi-index (NSeries (U (Listof String) ListofListofString) -> (U Flonum NSeries)))
(define (nseries-loc-multi-index nseries label)
  (unless (NSeries-index nseries)
    (let ((k (current-continuation-marks)))
      (raise (make-exn:fail:contract "nseries must have a label index." k))))

  (: get-index-val ((Listof String) -> Symbol))
  (define (get-index-val label)
    (string->symbol (string-append (string-join label "\t") "\t")))
  
  (if (ListofListofString? label)
      (nseries-loc nseries (map get-index-val label))
      (nseries-loc nseries (get-index-val label))))
    
(: nseries-loc (NSeries (U Label (Listof Label) (Listof Boolean)) -> (U Flonum NSeries)))
(define (nseries-loc nseries label)
  (unless (is-indexed? (assert (NSeries-index nseries)))
    (let ((k (current-continuation-marks)))
      (raise (make-exn:fail:contract "nseries must have a label index." k))))

  (if (ListofBoolean? label)
      (nseries-loc-boolean nseries label)
      (let ((associated-indices-length : (Listof Integer)
                                       (map (lambda ([l : Label]) (length (nseries-index-ref nseries l))) (convert-to-label-lst label)))
            (vals : FlVector
             (if (list? label)
                 (list->flvector (assert (flatten (map (lambda ([l : Label]) (nseries-index-ref nseries l)) label)) ListofFlonum?))
                 (list->flvector (assert (nseries-index-ref nseries label) ListofFlonum?)))))

        (if (= (flvector-length vals) 1)
            (flvector-ref vals 0)
            (new-NSeries vals #:index (build-index-from-list (build-labels-by-count (convert-to-label-lst label) associated-indices-length)))))))

; index based
(: nseries-iloc (NSeries (U Index (Listof Index)) -> (U Flonum NSeries)))
(define (nseries-iloc nseries idx)
  (let ((referencer (nseries-referencer nseries)))
    (if (list? idx)
        ; get labels from RFIndex that refer to given indicies
        ; make a new index from these labels using build-index-from-labels
        ; sub-vector the data vector to get the data and create a new-BSeries
        (new-NSeries
         (list->flvector (for/list: : (Listof Flonum) ([i idx])
                           (referencer (assert i index?))))
         #:index (if (not (NSeries-index nseries))
                     #f
                     (build-index-from-list (map (lambda ([i : Index]) (idx->key (assert (NSeries-index nseries)) i)) idx))))
        (referencer idx))))

(: nseries-iloc-range (NSeries Index Index -> NSeries))
(define (nseries-iloc-range nseries start end)
  ; use vector-copy library method
  (new-NSeries
   (flvector-copy (nseries-data nseries) start end)
   #:index (if (not (NSeries-index nseries))
               #f
               (build-index-from-list (map (lambda ([i : Index]) (idx->key (assert (NSeries-index nseries)) i)) (range start end))))))

; ***********************************************************

; ***********************************************************
;; NSeries groupby

(define-type Key String)
(define-type GroupHash (HashTable Key (Listof Flonum)))

; This function is self-explanatory, it consumes no arguments
; and creates a hash map which will represent a JoinHash.
(: make-group-hash (-> GroupHash))
(define (make-group-hash)
  (make-hash))

;Used to determine the groups for the groupby. If by is a function, it’s called on each value of the object’s index. The Series VALUES will be used to determine the groups.
(: nseries-groupby (NSeries [#:by-value Boolean] -> GroupHash))
(define (nseries-groupby nseries #:by-value [by-value #f])
  (define: group-index : GroupHash (make-group-hash))  

  (let ((len (nseries-length nseries))
        (k (current-continuation-marks)))
    (if (zero? len)
	(raise (make-exn:fail:contract "nseries can't be empty on groupby." k))
	(begin          
	  (do ((i 0 (add1 i)))
	      ((>= i len) group-index)
	    (let* ((flonum-val : (U Flonum NSeries) (nseries-iloc nseries (assert i index?)))
                   (flonum-list : (Listof Flonum) (if (flonum? flonum-val)
                                                      (list flonum-val)
                                                      (flvector->list (NSeries-data (assert flonum-val NSeries?)))))
                   (key (if by-value
                            (nseries-iloc nseries (assert i index?))
                            (if (NSeries-index nseries)
                                (idx->key (assert (NSeries-index nseries)) (assert i index?))
                                (assert i index?))))
                   (key-str : String (cond
                                       [(symbol? key) (symbol->string key)]
                                       [(number? key) (number->string key)]
                                       ; pretty-format anything else
                                       [else (pretty-format key)])))              
              (hash-update! group-index key-str
                            (λ: ((val : (Listof Flonum)))                                
                              (append flonum-list val))
                            (λ () (list)))))))))

; ***********************************************************
;; NSeries agg ops
(define-type AggValueHash (Mutable-HashTable Key GenericType))

; Applies the aggregate function specificed by function-name to the values in
; the column-name column. Currently supports 3: sum, avg, count.
(: apply-agg-nseries (Symbol GroupHash -> GenSeries))
(define (apply-agg-nseries function-name group-hash)
  (define len (hash-count group-hash))

  (: agg-value-hash AggValueHash)
  (define agg-value-hash (make-hash))

  (hash-for-each group-hash
                 (lambda ([key : String] [val : (Listof Flonum)])
                   
                   (let ((key (assert key string?))
                         (val (assert (flatten val) ListofFlonum?)))
                     (hash-set! agg-value-hash key
                                (cond 
                                  [(eq? function-name 'sum) (apply + val)]
                                  [(eq? function-name 'mean) (mean val)]
                                  [(eq? function-name 'median) (median < val)]
                                  [(eq? function-name 'mode) (most-frequent-element val)]
                                  [(eq? function-name 'count) (length val)]
                                  [(eq? function-name 'min) (argmin (lambda ([x : Real]) x) val)]
                                  [(eq? function-name 'max) (argmax (lambda ([x : Real]) x) val)]
                                  [else (error 'apply-agg-data-frame "Unknown aggregate function.")])))))

  (agg-value-hash-to-gen-series agg-value-hash))

; ***********************************************************
(: nseries-index-from-predicate (NSeries (Flonum -> Boolean) -> RFIndex))
(define (nseries-index-from-predicate nseries pred)  
  (build-index-from-list
   (for/list : (Listof IndexDataType)
     ([val (flvector->list (nseries-data nseries))]
      [n (in-naturals)]
      #:when (pred (assert val flonum?)))
     (if (nseries-index nseries)
         (idx->key (assert (nseries-index nseries)) (assert n index?))
         (assert n index?)))))

(: nseries-index-from-predicate-not (NSeries (Flonum -> Boolean) -> RFIndex))
(define (nseries-index-from-predicate-not nseries pred)  
  (build-index-from-list
   (for/list : (Listof IndexDataType)
     ([val (flvector->list (nseries-data nseries))]
      [n (in-naturals)]
      #:when (not (pred (assert val flonum?))))
     (if (nseries-index nseries)
         (idx->key (assert (nseries-index nseries)) (assert n index?))
         (assert n index?)))))

(: nseries-data-idxes-from-predicate (NSeries (Flonum -> Boolean) -> (Listof Index)))
(define (nseries-data-idxes-from-predicate nseries pred)    
   (for/list : (Listof Index)
     ([val (flvector->list (nseries-data nseries))]
      [n (in-naturals)]
      #:when (pred (assert val flonum?)))
         (assert n index?)))

(: nseries-data-idxes-from-predicate-not (NSeries (Flonum -> Boolean) -> (Listof Index)))
(define (nseries-data-idxes-from-predicate-not nseries pred)    
   (for/list : (Listof Index)
     ([val (flvector->list (nseries-data nseries))]
      [n (in-naturals)]
      #:when (not (pred (assert val flonum?))))
         (assert n index?)))

(: nseries-filter (NSeries (Flonum -> Boolean) -> NSeries))
(define (nseries-filter nseries filter-function)
  ; need to use new filtered data to get the new index
  ; setting #f is naive
  (new-NSeries (filter filter-function (flvector->list (nseries-data nseries))) #:index (nseries-index-from-predicate nseries filter-function)))

(: nseries-filter-not (NSeries (Flonum -> Boolean) -> NSeries))
(define (nseries-filter-not nseries filter-function)
  (new-NSeries (filter-not filter-function (flvector->list (nseries-data nseries))) #:index (nseries-index-from-predicate-not nseries filter-function)))
; ***********************************************************