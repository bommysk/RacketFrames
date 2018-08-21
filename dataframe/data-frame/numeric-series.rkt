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

(provide:
 [set-NSeries-index (NSeries (U (Listof Label) SIndex) -> NSeries)]
 [nseries-iref (NSeries (Listof Index) -> (Listof Float))]
 [nseries-loc-boolean (NSeries (Listof Boolean) -> (U Float NSeries))]
 [nseries-loc (NSeries (U Label (Listof Label) (Listof Boolean)) -> (U Float NSeries))]
 [nseries-iloc (NSeries (U Index (Listof Index)) -> (U Float NSeries))]
 [nseries-label-ref (NSeries Label -> (Listof Float))]
 [nseries-range (NSeries Index -> FlVector)]
 [nseries-referencer (NSeries -> (Index -> Float))]
 [nseries-length (NSeries -> Index)]
 [nseries-data (NSeries -> FlVector)]
 [map/ns (NSeries (Float -> Float) -> NSeries)]
 [bop/ns (NSeries NSeries (Float Float -> Float) -> NSeries)]
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
 [+./ns (NSeries Float -> NSeries)]
 [-./ns (NSeries Float -> NSeries)]
 [*./ns (NSeries Float -> NSeries)]
 [/./ns (NSeries Float -> NSeries)]
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
 [apply-agg-ns (Symbol NSeries -> Real)]
 [apply-stat-ns (Symbol NSeries -> Real)]
 [flvector->list (FlVector Fixnum -> (Listof Float))]
 [list->flvector ((Listof Float) -> FlVector)]
 [nseries-print (NSeries Output-Port -> Void)])

(provide
 ;flvector-print
 (struct-out NSeries)
 new-NSeries)

; ***********************************************************

; ***********************************************************

(require
 racket/unsafe/ops
 racket/flonum
 (only-in "settings.rkt"
	  Settings-decimals
	  Settings-max-output
	  settings)
 (only-in "indexed-series.rkt"
	  label-index label->lst-idx
	  build-index-from-labels
	  Label SIndex
	  LabelIndex LabelIndex-index
          idx->label is-labeled?)
 (only-in "boolean-series.rkt"
          BSeries BSeries-data)
 (only-in "integer-series.rkt"
          ISeries ISeries-data))

; ***********************************************************

; ***********************************************************
(: nseries-print (NSeries Output-Port -> Void))
(define (nseries-print nseries port)
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
              (if (LabelIndex-index nseries)
                  (display (idx->label nseries (assert i index?)) port)
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

(struct: Summary ([mean : Float]
                  [variance : Float]
                  [min : Float]
                  [max : Float]
                  [count : Natural]
                  [nans : Natural]))

;; An NSeries is an optimized Series for computation over vectors of Float
;; i.e., NSeries should be faster then (Series Float)
(struct: NSeries LabelIndex ([data : FlVector])
	 ;;  #:methods gen:custom-write [(define write-proc writer-NSeries)]
	 )

(: new-NSeries (FlVector (Option (U (Listof Label) SIndex)) -> NSeries))
(define (new-NSeries data labels)

  (: check-mismatch (SIndex -> Void))
  (define (check-mismatch index)
    (unless (eq? (flvector-length data) (hash-count index))
	    (let ((k (current-continuation-marks)))
	      (raise (make-exn:fail:contract "Cardinality of a Series' data and labels must be equal" k))))
    (void))

  (if (hash? labels)
      (begin
	(check-mismatch labels)
	(NSeries labels data))
      (if labels
	  (let ((index (build-index-from-labels labels)))
	    (check-mismatch index)
	    (NSeries index data))
	  (NSeries #f data))))

; ***********************************************************

; ***********************************************************
(: set-NSeries-index (NSeries (U (Listof Label) SIndex) -> NSeries))
(define (set-NSeries-index nseries labels)

  (define data (NSeries-data nseries))
  
  (: check-mismatch (SIndex -> Void))
  (define (check-mismatch index)
    (unless (eq? (flvector-length data) (hash-count index))
      (let ((k (current-continuation-marks)))
	(raise (make-exn:fail:contract "Cardinality of a Series' data and labels must be equal" k))))
    (void))

  (if (hash? labels)
      (begin
	(check-mismatch labels)
	(NSeries labels data))
      (let ((index (build-index-from-labels labels)))
        (check-mismatch index)
        (NSeries index data))))
; ***********************************************************

; ***********************************************************

; This function consumes an integer series and returns a
; lambda function which consumes an index and provides the
; value of the data at that index in the series. It can be
; defined once and used repeatedly as a referencer.
(: nseries-referencer (NSeries -> (Index -> Float)))
(define (nseries-referencer series)
  (let ((data (NSeries-data series)))
    (λ: ((idx : Index))
	(flvector-ref data idx))))

; This function consumes an integer series and an index and
; returns the value at that index in the series.
(: nseries-iref (NSeries (Listof Index) -> (Listof Float)))
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

(: nseries-label-ref (NSeries Label -> (Listof Float)))
(define (nseries-label-ref series label)
  (nseries-iref series (label->lst-idx series label)))

; This function consumes a numeric series and returns its
; data vector.
(: nseries-data (NSeries -> FlVector))
(define (nseries-data series)
  (NSeries-data series))

(: nseries-length (NSeries -> Index))
(define (nseries-length nseries)
  (flvector-length (NSeries-data nseries)))

(: flvector->list (FlVector Fixnum -> (Listof Float)))
(define (flvector->list flvec idx)
  (cond
    [(= idx (flvector-length flvec)) null]
    [else (cons (flvector-ref flvec idx) (flvector->list flvec (unsafe-fx+ idx 1)))]))

(: list->flvector ((Listof Float) -> FlVector))
(define (list->flvector float-list)
  (define len : Index (length float-list))

  (define result-flvector (make-flvector len))

  (for([flo float-list]
     [i (in-range len)])
    (flvector-set! result-flvector i flo))

  result-flvector)

; ***********************************************************

; ***********************************************************
;; Map a function

; This function consumes a series and a function both of Float
; types and applies the function to each member of the series
; returning a new series.
(: map/ns (NSeries (Float -> Float) -> NSeries))
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
    (NSeries (LabelIndex-index series) new-data)))

; ***********************************************************

; ***********************************************************

;; Binary NSeries Ops

(: bop/ns (NSeries NSeries (Float Float -> Float) -> NSeries))
(define (bop/ns ns1 ns2 bop)
  (define v1 (NSeries-data ns1))
  (define v2 (NSeries-data ns2))
  (define: len : Index (flvector-length v1))
  
  (unless (eqv? len (flvector-length v2))
	  (error '+/ns "Series must be of equal length."))
  
  (define: v-bop : FlVector (make-flvector len))

  (do: : NSeries ([idx : Fixnum 0 (unsafe-fx+ idx 1)])
       ((= idx len) (NSeries #f v-bop))
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

(: bop/ns/is (NSeries ISeries (Float Float -> Float) -> NSeries))
(define (bop/ns/is ns1 ns2 bop)
  (define v1 (NSeries-data ns1))
  (define v2 (ISeries-data ns2))
  (define: len : Index (flvector-length v1))
  
  (unless (eqv? len (vector-length v2))
	  (error 'bop/ns "Series must be of equal length."))
  
  (define: v-bop : FlVector (make-flvector len))

  (do: : NSeries ([idx : Fixnum 0 (unsafe-fx+ idx 1)])
       ((= idx len) (NSeries #f v-bop))
       (flvector-set! v-bop idx (bop (flvector-ref v1 idx)
				 (exact->inexact (vector-ref v2 idx))))))

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

(: bop/is/ns (ISeries NSeries (Float Float -> Float) -> NSeries))
(define (bop/is/ns ns1 ns2 bop)
  (define v1 (ISeries-data ns1))
  (define v2 (NSeries-data ns2))

  (define: len : Index (vector-length v1))
  
  (unless (eqv? len (flvector-length v2))
	  (error 'bop/ns "Series must be of equal length."))
  
  (define: v-bop : FlVector (make-flvector len))

  (do: : NSeries ([idx : Fixnum 0 (unsafe-fx+ idx 1)])
       ((= idx len) (NSeries #f v-bop))
       (flvector-set! v-bop idx (bop (exact->inexact (vector-ref v1 idx))
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

(: bop./ns (Float NSeries (Float Float -> Float) -> NSeries))
(define (bop./ns fl ns bop)
  (define v1 (NSeries-data ns))
  (define: len : Index (flvector-length v1))
  (define: v-bop : FlVector (make-flvector len))

  (do: : NSeries ([idx : Fixnum 0 (unsafe-fx+ idx 1)])
       ((= idx len) (NSeries #f v-bop))
       (flvector-set! v-bop idx (bop (flvector-ref v1 idx) fl))))

(: +./ns (NSeries Float -> NSeries))
(define (+./ns ns fl)
  (bop./ns fl ns fl+))

(: -./ns (NSeries Float -> NSeries))
(define (-./ns ns fl )
  (bop./ns fl ns fl-))

(: *./ns (NSeries Float -> NSeries))
(define (*./ns ns fl)
  (bop./ns fl ns fl*))

(: /./ns (NSeries Float -> NSeries))
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
(: comp/ns (NSeries NSeries (Float Float -> Boolean) -> BSeries))
(define (comp/ns ns1 ns2 comp)
  (define v1 (NSeries-data ns1))
  (define v2 (NSeries-data ns2))
  (define: len : Index (flvector-length v1))
  
  (unless (eqv? len (flvector-length v2))
	  (error 'comp/ns "Series must be of equal length."))
  
  (define: v-comp : (Vectorof Boolean) (make-vector len #f))

  ; Do loop returns ISeries, idx to 0 and increments by 1 Fixnum on
  ; each iteration (this is the step-exprs). When the loop has gone
  ; through the whole vector, the resulting new ISeries is returned
  ; which the v-bop as the data.
  (do: : BSeries ([idx : Fixnum 0 (unsafe-fx+ idx #{1 : Fixnum})])
       ((= idx len) (BSeries #f v-comp))
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
  (comp/ns ns1 ns2 (lambda ([a : Float] [b : Float]) (not (unsafe-fl= a b)))))

; ***********************************************************


; ***********************************************************
;; Binary NSeries comp

; This function consumes 2 integer series and a function which
; consumes 2 Fixnum's and produces a Boolean result for comparison.
; This function is applied to each value in the 2 series at the same
; index resulting in a new boolean point and at the end of the loop
; a new data vector. This data vector is the data of the new ISeries
; which is returned.
(: comp/ns/is (NSeries ISeries (Float Float -> Boolean) -> BSeries))
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
       ((= idx len) (BSeries #f v-comp))
       (vector-set! v-comp idx (comp (flvector-ref v1 idx)
				   (exact->inexact (vector-ref v2 idx))))))

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
  (comp/ns/is ns is (lambda ([a : Float] [b : Float]) (not (unsafe-fl= a b)))))

; ***********************************************************

; ***********************************************************
;; Binary NSeries comp

; This function consumes 2 integer series and a function which
; consumes 2 Fixnum's and produces a Boolean result for comparison.
; This function is applied to each value in the 2 series at the same
; index resulting in a new boolean point and at the end of the loop
; a new data vector. This data vector is the data of the new ISeries
; which is returned.
(: comp/is/ns (ISeries NSeries (Float Float -> Boolean) -> BSeries))
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
       ((= idx len) (BSeries #f v-comp))
       (vector-set! v-comp idx (comp (exact->inexact (vector-ref v1 idx))
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
  (comp/is/ns is ns (lambda ([a : Float] [b : Float]) (not (unsafe-fl= a b)))))

; ***********************************************************

; ***********************************************************
;; NSeries agg ops

; Applies the aggregate function specificed by function-name to the values in
; the column-name column. Currently supports 3: sum, avg, count.
(: apply-agg-ns (Symbol NSeries -> Real))
(define (apply-agg-ns function-name series)
  (cond 
    [(eq? function-name 'sum) (apply + (flvector->list (NSeries-data series) 0))]
    [(eq? function-name 'mean) (mean (flvector->list (NSeries-data series) 0))]
    ;[(eq? function-name 'median) (median (flvector->list (ISeries-data series)))]
    [(eq? function-name 'count) (nseries-length series)]
    ;[(eq? function-name 'min) (flvector-argmin (lambda (x) x) (ISeries-data series))]
    ;[(eq? function-name 'max) (flvector-argmax (lambda (x) x) (ISeries-data series))]
    [else (error 'apply-agg-ns "Unknown aggregate function.")]))

; ***********************************************************

; ***********************************************************
;; NSeries stat ops

(: apply-stat-ns (Symbol NSeries -> Real))
(define (apply-stat-ns function-name series)
  (cond 
    [(eq? function-name 'variance) (variance (flvector->list (NSeries-data series) 0))]
    [(eq? function-name 'stddev) (stddev (flvector->list (NSeries-data series) 0))]
    [(eq? function-name 'skewness) (skewness (flvector->list (NSeries-data series) 0))]
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
(define-predicate ListofFloat? (Listof Float))

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

(: nseries-loc-boolean (NSeries (Listof Boolean) -> (U Float NSeries)))
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
          (new-NSeries (flvector) #f))
       
      (for ([b boolean-lst]
            [d (flvector->list data (flvector-length data))])
        (begin
          (when b
            (begin              
              (flvector-set! new-data new-data-idx (flvector-ref data data-idx))
              (set! new-data-idx (add1 new-data-idx))))
          (set! data-idx (add1 data-idx)))))

  (if (= (flvector-length new-data) 1)
      (flvector-ref new-data 0)
      (new-NSeries new-data #f)))
    
(: nseries-loc (NSeries (U Label (Listof Label) (Listof Boolean)) -> (U Float NSeries)))
(define (nseries-loc nseries label)
  (unless (is-labeled? nseries)
    (let ((k (current-continuation-marks)))
      (raise (make-exn:fail:contract "nseries must have a label index." k))))

  (if (ListofBoolean? label)
      (nseries-loc-boolean nseries label)
      (let ((associated-indices-length : (Listof Integer)
                                       (map (lambda ([l : Label]) (length (nseries-label-ref nseries l))) (convert-to-label-lst label)))
            (vals : FlVector
             (if (list? label)
                 (list->flvector (assert (flatten (map (lambda ([l : Label]) (nseries-label-ref nseries l)) label)) ListofFloat?))
                 (list->flvector (assert (nseries-label-ref nseries label) ListofFloat?)))))

        (if (= (flvector-length vals) 1)
            (flvector-ref vals 0)
            (new-NSeries vals (build-index-from-labels (build-labels-by-count (convert-to-label-lst label) associated-indices-length)))))))

; index based
(: nseries-iloc (NSeries (U Index (Listof Index)) -> (U Float NSeries)))
(define (nseries-iloc nseries idx)
  (let ((referencer (nseries-referencer nseries)))
    (if (list? idx)
        ; get labels from SIndex that refer to given indicies
        ; make a new index from these labels using build-index-from-labels
        ; sub-vector the data vector to get the data and create a new-BSeries
        (new-NSeries
         (list->flvector (for/list: : (Listof Float) ([i idx])
                           (flvector-ref (nseries-data nseries) i)))
         (build-index-from-labels (map (lambda ([i : Index]) (idx->label nseries i)) idx)))
        (referencer idx))))

; ***********************************************************