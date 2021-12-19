;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*    Author: Shubham Kahal
;*    File: integer-series.rkt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket
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
 (struct-out ISeries)
 ISeries-index RFFixnum RFFixnum? (rename-out [GroupHash iseries-grouphash] [DEFAULT_NULL_VALUE ISERIES_DEFAULT_NULL_VALUE]))
(provide:
 [new-ISeries ((U FxVector (Vectorof Fixnum) (Sequenceof Fixnum) (Sequenceof RFFixnum)) [#:index (Option (U (Sequenceof IndexDataType) RFIndex))]
                                 [#:fill-null RFNULL] [#:sort Boolean] [#:encode Boolean] -> ISeries)]
 [set-ISeries-index (ISeries (U (Sequenceof IndexDataType) RFIndex) -> ISeries)]
 ; in Pandas, fillna
 [set-ISeries-null-value (ISeries RFNULL -> ISeries)]
 [set-ISeries-fixnum-null-value-inplace (ISeries Fixnum -> Void)]
 [iseries-iref (ISeries (Listof Index) -> (Listof RFFixnum))]
 [iseries-loc-boolean (ISeries (Listof Boolean) -> (U RFFixnum ISeries))]
 [iseries-loc (ISeries (U Label (Listof Label) (Listof Boolean)) -> (U RFFixnum ISeries))]
 [iseries-loc-multi-index (ISeries (U (Listof String) ListofListofString) -> (U RFFixnum ISeries))]
 [iseries-iloc (ISeries (U Index (Listof Index)) -> (U RFFixnum ISeries))]
 [iseries-iloc-range (ISeries Index Index -> ISeries)]
 [iseries-index-ref (ISeries IndexDataType -> (Listof RFFixnum))]
 [iseries-range (ISeries Index Index -> (Vectorof RFFixnum))]
 [iseries-length (ISeries -> Index)]
 [iseries-referencer (ISeries -> (Index -> RFFixnum))]
 [iseries-data (ISeries -> (Vectorof RFFixnum))]
 [in-iseries (RFFixnum ISeries -> Boolean)]
 [iseries-index (ISeries -> (U False RFIndex))]
 [iseries-null-value (ISeries -> Fixnum)]
 [iseries-custom-null-value (ISeries -> RFNULL)]
 [iseries-value-is-null? (ISeries Fixnum -> Boolean)]
 [iseries-groupby (ISeries [#:by-value Boolean] -> GroupHash)]
 [apply-agg-iseries (Symbol GroupHash -> GenSeries)]
 [map/is (ISeries (Fixnum -> Fixnum) -> ISeries)]
 [bop/is (ISeries ISeries (Fixnum Fixnum -> Fixnum) -> ISeries)]
 [comp/is (ISeries ISeries (Fixnum Fixnum -> Boolean) -> BSeries)]
 [+/is (ISeries ISeries -> ISeries)]
 [-/is (ISeries ISeries -> ISeries)]
 [*/is (ISeries ISeries -> ISeries)]
 [//is (ISeries ISeries -> ISeries)]
 [%/is (ISeries ISeries -> ISeries)]
 [r/is (ISeries ISeries -> ISeries)]
 [+./is (ISeries Fixnum -> ISeries)]
 [-./is (ISeries Fixnum -> ISeries)]
 [*./is (ISeries Fixnum -> ISeries)]
 [/./is (ISeries Fixnum -> ISeries)]
 [%./is (ISeries Fixnum -> ISeries)]
 [r./is (ISeries Fixnum -> ISeries)]
 [>/is (ISeries ISeries -> BSeries)]
 [</is (ISeries ISeries -> BSeries)]
 [>=/is (ISeries ISeries -> BSeries)]
 [<=/is (ISeries ISeries -> BSeries)]
 [=/is (ISeries ISeries -> BSeries)]
 [!=/is (ISeries ISeries -> BSeries)]
 [>./is (ISeries Fixnum -> BSeries)]
 [<./is (ISeries Fixnum -> BSeries)]
 [>=./is (ISeries Fixnum -> BSeries)]
 [<=./is (ISeries Fixnum -> BSeries)]
 [=./is (ISeries Fixnum -> BSeries)]
 [!=./is (ISeries Fixnum -> BSeries)]
 [apply-agg-is (Symbol ISeries -> GenericType)]
 [apply-stat-is (Symbol ISeries -> Real)]
 [iseries-print (ISeries [#:output-port Output-Port] -> Void)]
 [iseries-filter (ISeries (RFFixnum -> Boolean) -> ISeries)]
 [iseries-filter-not (ISeries (RFFixnum -> Boolean) -> ISeries)]
 [fxvector->list (FxVector Fixnum -> (Listof Fixnum))]
 [list->fxvector ((Listof Fixnum) -> FxVector)]
 [iseries-notna (ISeries -> ISeries)]
 [iseries-isna (ISeries -> ISeries)]
 [make-RFFixnum-vector ((U (Sequenceof Fixnum) (Sequenceof RFFixnum)) -> (Vectorof RFFixnum))]
 (derive-fixnum-value (ISeries RFFixnum -> Fixnum)))
; ***********************************************************

; ***********************************************************
; use build-index-from-labels function and Label, SIndex and
; LabelIndex structs from indexed-series.
(require
  racket/vector
  racket/pretty
  racket/fixnum
  racket/unsafe/ops
  math/statistics
  "../util/data-encode.rkt"
  (only-in "indexed-series.rkt"
           build-index-from-list build-multi-index-from-list         
           RFIndex RFIndex? RFNULL IndexDataType RFNoData RFNoData?
           extract-index
           Label SIndex LabelIndex LabelIndex-index
           FIndex FlonumIndex
           label-index key->lst-idx
           idx->key is-labeled? ListofIndexDataType?
           is-indexed? ListofIndex? ListofListofString ListofListofString?
           key-delimiter)
  (only-in "boolean-series.rkt"
           new-BSeries BSeries)
  (only-in "generic-series.rkt"
           GenSeries GenSeries? GenericType gen-series-iref new-GenSeries
           gen-series-referencer)
  (only-in "groupby-util.rkt"
           make-agg-value-hash-sindex agg-value-hash-to-gen-series AggValueHash))
; ***********************************************************

; ***********************************************************
; racket/fixnum library provides operations like fx+ that
; consumes and produce only fixnums. The operations in this
; library are meant to be safe versions of unsafe operations
; like unsafe-fx+. These safe operations are generally no
; faster than using generic primitives like +. But they are
; slower than the unsafe versions, with the benefit of being
; safer. This library will be using unsafe operations for
; speed improvement.
(define-type RFFixnum (U Fixnum RFNoData))
(define-predicate RFFixnum? RFFixnum)
(define DEFAULT_NULL_VALUE : Fixnum 0)
;; Integer series optimized with use of Fixnum.
(struct ISeries ([index : (Option RFIndex)]
                 [data : (Vectorof RFFixnum)]
                 ; when the null-value is not a fixnum?, the fixnum-null-value is set to 0
                 [null-value : RFNULL]
                 ; needed for type checking purposes and to do proper arithmetic operations in numeric series
                 [fixnum-null-value : Fixnum]
                 ; encode data by element count to optimze memory storage and read/write operations
                 ; when data vector lacks variety
                 [encoded : Boolean]
                 [data-count-encoded : (Option (Listof (Pairof Any Real)))])
  #:mutable
  #:transparent)

(: make-RFFixnum-vector ((U (Sequenceof Fixnum) (Sequenceof RFFixnum)) -> (Vectorof RFFixnum)))
(define (make-RFFixnum-vector seq)
  (let ((vec : (Vectorof RFFixnum) (list->vector (sequence->list seq))))
    vec))

; Consumes a Vector of Fixnum or RFFixnum and a list of Labels which
; can come in list form or SIndex form and produces a ISeries
; struct object.

; sorting data vector results in more efficient
; sparse index, but it is up to the user and user case
                 
; When this is set we run length encode on save
; it as the vector data and generate the index
; to match
(: new-ISeries ((U (Vectorof Fixnum) FxVector (Sequenceof Fixnum) (Sequenceof RFFixnum)) [#:index (Option (U (Sequenceof IndexDataType) RFIndex))]
                                  [#:fill-null RFNULL] [#:sort Boolean] [#:encode Boolean] -> ISeries))
(define (new-ISeries data #:index [labels #f] #:fill-null [null-value DEFAULT_NULL_VALUE] #:sort [do-sort #f] #:encode [encode #f])
  (define RFFixnum-vector : (Vectorof RFFixnum) (make-RFFixnum-vector data))
  
  (: check-mismatch (RFIndex -> Void))
  (define (check-mismatch index)    
    (let ((index-length (apply + (for/list: : (Listof Index)
                                   ([value (in-hash-values (extract-index index))])
                                   (length (assert value ListofIndex?))))))

      (unless (eq? (vector-length RFFixnum-vector) index-length)
        (let ((k (current-continuation-marks)))
          (raise (make-exn:fail:contract "Cardinality of a Series' data and labels must be equal" k))))
      (void)))

  ; encode data by element count to avoid repetition if user elects to sort or encode series
  (let*: ((data-count-encoded : (Option (Listof (Pairof Any Real))) (if (or do-sort encode) (most-frequent-element-list RFFixnum-vector) #f))
          (data-vector : (Vectorof RFFixnum)
                      (if (not data-count-encoded)
                          RFFixnum-vector
                          (let ((mf-elements : (Listof Any) (most-frequent-elements data-count-encoded)))
                            (for/vector: : (Vectorof RFFixnum) ([el mf-elements]) (assert el RFFixnum?)))))
          (fixnum-null-value : Fixnum (if (fixnum? null-value) null-value DEFAULT_NULL_VALUE)))
    (if (RFIndex? labels)      
        (ISeries labels data-vector null-value fixnum-null-value encode data-count-encoded)
        (if labels
            (let ((index (build-index-from-list (assert labels ListofIndexDataType?))))
              (check-mismatch index)
              (ISeries index data-vector null-value fixnum-null-value encode data-count-encoded))
            (ISeries #f data-vector null-value fixnum-null-value encode data-count-encoded)))))

; ***********************************************************

; ***********************************************************
; Any non matching indexes need to have their values set to NULL
(: set-ISeries-index (ISeries (U (Sequenceof IndexDataType) RFIndex) -> ISeries))
(define (set-ISeries-index iseries labels)
  (new-ISeries (iseries-data iseries) #:index labels #:fill-null (iseries-null-value iseries)))

(: set-ISeries-null-value (ISeries RFNULL -> ISeries))
(define (set-ISeries-null-value iseries null-value)
  (new-ISeries (iseries-data iseries) #:index (iseries-index iseries) #:fill-null null-value))

(: set-ISeries-fixnum-null-value-inplace (ISeries Fixnum -> Void))
(define (set-ISeries-fixnum-null-value-inplace iseries null-value)
  (set-ISeries-fixnum-null-value! iseries null-value))
; ***********************************************************

; ***********************************************************

(: iseries-print (ISeries [#:output-port Output-Port] -> Void))
(define (iseries-print iseries #:output-port [port (current-output-port)])
  (define v (iseries-data iseries))
  (let ((len (vector-length v))
        (out (current-output-port)))
    (if (zero? len)
        (displayln "Empty $ISeries" port)
        (begin
          (displayln "*********" port)
          (displayln "$ISeries" port)
          (displayln "*********" port)
          (do ((i 0 (add1 i)))
            ((>= i len) (void))
            (let ((num (if (iseries-value-is-null? iseries (vector-ref v i)) (iseries-custom-null-value iseries) (vector-ref v i))))
              (if (iseries-index iseries)
                  (display (idx->key (assert (iseries-index iseries)) (assert i index?)) port)
                  (display (assert i index?) port))
              (display " " port)
              (displayln num port)))))))
; ***********************************************************

; ***********************************************************
; This function consumes an integer series and returns a
; lambda function which consumes an index and provides the
; value of the data at that index in the series. It can be
; defined once and used repeatedly as a referencer.
(: iseries-referencer (ISeries -> (Index -> RFFixnum)))
(define (iseries-referencer iseries)
  (let ((data (iseries-data iseries)))
    (λ: ((idx : Index))
  (vector-ref data idx))))

; This function consumes an integer series and an index and
; returns the value at that index in the series.
(: iseries-iref (ISeries (Listof Index) -> (Listof RFFixnum)))
(define (iseries-iref series lst-idx)
  (map (lambda ((idx : Index)) (vector-ref (iseries-data series) idx))
       lst-idx))

; This function consumes an integer series and an index and
; returns a vector of values in the range [0:index] in the series.
(: iseries-range (ISeries Index Index -> (Vectorof RFFixnum)))
(define (iseries-range series start end)
   (vector-copy (iseries-data series) start end))

; This function consumes an integer series and returns its
; data vector.
(: iseries-data (ISeries -> (Vectorof RFFixnum)))
(define (iseries-data series)
  (ISeries-data series))

; This function consumes an integer series and returns its
; data vector with RFNoData replaced wtih the fixnum null-value.
(: iseries-data-fill-fixnum-null-value (ISeries -> (Vectorof Fixnum)))
(define (iseries-data-fill-fixnum-null-value series)
  (vector-map (λ: ((val : RFFixnum)) (if (RFNoData? val) (iseries-null-value series) val)) (iseries-data series)))

; This function consumes an integer series and returns its
; data vector with RFNoData replaced wtih the custom-null-value.
(: iseries-data-fill-custom-null-value (ISeries -> (Vectorof GenericType)))
(define (iseries-data-fill-custom-null-value series)
  (vector-map (λ: ((val : RFFixnum)) (if (RFNoData? val) (iseries-custom-null-value series) val)) (iseries-data series)))

; This function consumes an integer series and returns its
; index.
(: iseries-index (ISeries -> (U False RFIndex)))
(define (iseries-index series)
  (ISeries-index series))

; This function consumes an integer series and returns its
; data vector.
(: iseries-custom-null-value (ISeries -> RFNULL))
(define (iseries-custom-null-value series)
  (ISeries-null-value series))

; This function consumes an integer series and returns its
; data vector.
(: iseries-null-value (ISeries -> Fixnum))
(define (iseries-null-value series)
  (ISeries-fixnum-null-value series))

; This function consumes an integer series and a integer value
; and returns whether it is considered to be NULL in the series.
(: iseries-value-is-null? (ISeries RFFixnum -> Boolean))
(define (iseries-value-is-null? series value)
  (or (RFNoData? value) (eq? (iseries-null-value series) value)))

; This function consumes a series and an IndexDataType and returns
; the list of values at that index in the series.
(: iseries-index-ref (ISeries IndexDataType -> (Listof RFFixnum)))
(define (iseries-index-ref series item)
  (iseries-iref series (key->lst-idx (assert (iseries-index series)) item)))

; This function consumes an integer series and returns the
; length of that series.
(: iseries-length (ISeries -> Index))
(define (iseries-length series)
  (vector-length (ISeries-data series)))

(: in-iseries (RFFixnum ISeries -> Boolean))
(define (in-iseries val series)
  (if (vector-memq val (iseries-data series)) #t #f))

(: iseries-notna (ISeries -> ISeries))
(define (iseries-notna iseries)
  (iseries-filter iseries (lambda ((x : RFFixnum)) (not (RFNoData? x)))))

(: iseries-isna (ISeries -> ISeries))
(define (iseries-isna iseries)
  (iseries-filter iseries (lambda ((x : RFFixnum)) (RFNoData? x))))

(: fxvector->list (FxVector Fixnum -> (Listof Fixnum)))
(define (fxvector->list fxvec idx)
  (cond
    [(= idx (fxvector-length fxvec)) null]
    [else (cons (fxvector-ref fxvec idx) (fxvector->list fxvec (unsafe-fx+ idx 1)))]))

(: list->fxvector ((Listof Fixnum) -> FxVector))
(define (list->fxvector fixnum-list)
  (define len : Index (length fixnum-list))

  (define result-fxvector (make-fxvector len))

  (for([fix fixnum-list]
     [i (in-range len)])
    (fxvector-set! result-fxvector i (assert fix fixnum?)))

  result-fxvector)

; ***********************************************************

; ***********************************************************
(: map/is (ISeries (Fixnum -> Fixnum) -> ISeries))
(define (map/is iseries fn)
  (let* ((old-data : (Vectorof RFFixnum) (iseries-data (iseries-notna iseries)))
        (new-data : (Vectorof Fixnum)
                              (build-vector (vector-length old-data)
                                            (λ: ((idx : Index))
                                              (fn (derive-fixnum-value iseries (vector-ref old-data idx)))))))
    (new-ISeries new-data #:index (iseries-index iseries) #:fill-null (iseries-null-value iseries))))
; ***********************************************************

; ***********************************************************
;; Binary ISeries bops

; This function consumes 2 integer series and a function which
; consumes 2 Fixnum's and produces a Fixnum result. This function
; is applied to each value in the 2 series at the same index
; resulting in a new data point and at the end of the loop a new
; data vector. This data vector is the data of the new ISeries
; which is returned.
(: bop/is (ISeries ISeries (Fixnum Fixnum -> Fixnum) -> ISeries))
(define (bop/is is1 is2 bop)
  (define v1 (iseries-data (iseries-notna is1)))
  (define v2 (iseries-data (iseries-notna is2)))
  (define: len : Index (vector-length v1))
  
  (unless (eqv? len (vector-length v2))
    (error 'bop/is "Series must be of equal length."))
  
  (define: v-bop : (Vectorof Fixnum) (make-vector len #{0 : Fixnum}))

  ; Do loop returns ISeries, idx to 0 and increments by 1 Fixnum on
  ; each iteration (this is the step-exprs). When the loop has gone
  ; through the whole vector, the resulting new ISeries is returned
  ; which the v-bop as the data.
  (do: : ISeries ([idx : Fixnum 0 (unsafe-fx+ idx #{1 : Fixnum})])
       ((= idx len) (new-ISeries v-bop))
       (vector-set! v-bop idx (bop (derive-fixnum-value is1 (vector-ref v1 idx))
                                   (derive-fixnum-value is2 (vector-ref v2 idx))))))
; ***********************************************************

; ***********************************************************
; Functions provided by racket/unsafe/ops are unsafe. They
; have certain constraints, but the constraints are not
; checked, which allows the system to generate and execute
; faster code. If arguments violate an unsafe function’s
; constraints, the function’s behavior and result is
; unpredictable, and the entire system can crash or become
; corrupted.
; ***********************************************************

; ***********************************************************
; These functions apply addition, subtraction, multiplication
; and division using unsafe-fx and the bop/is function defined
; above.

(: +/is (ISeries ISeries -> ISeries))
(define (+/is is1 is2)
  (bop/is is1 is2 unsafe-fx+))

(: -/is (ISeries ISeries -> ISeries))
(define (-/is ns1 ns2)
  (bop/is ns1 ns2 unsafe-fx-))

(: */is (ISeries ISeries -> ISeries))
(define (*/is ns1 ns2)
  (bop/is ns1 ns2 unsafe-fx*))

(: //is (ISeries ISeries -> ISeries))
(define (//is ns1 ns2)
  (bop/is ns1 ns2 unsafe-fxquotient))

(: r/is (ISeries ISeries -> ISeries))
(define (r/is ns1 ns2)
  (bop/is ns1 ns2 unsafe-fxremainder))

(: %/is (ISeries ISeries -> ISeries))
(define (%/is ns1 ns2)
  (bop/is ns1 ns2 unsafe-fxmodulo))

; ***********************************************************

; ***********************************************************
;; Scalar ISeries bops

; This function consumes a Fixnum and an integer series and
; a binary operation function which
; consumes 2 Fixnum's and produces a Fixnum result. This function
; is applied to each value in the 2 series at the same index
; resulting in a new data point and at the end of the loop a new
; data vector. This data vector is the data of the new ISeries
; which is returned.

(: bop./is (Fixnum ISeries (Fixnum Fixnum -> Fixnum) -> ISeries))
(define (bop./is fx is bop)
  (define: v1 : (Vectorof RFFixnum) (iseries-data (iseries-notna is)))
  (define: len : Index (vector-length v1))
  (define: v-bop : (Vectorof RFFixnum) ((inst make-vector RFFixnum) len #{0 : Fixnum}))

  (do: : ISeries ([idx : Fixnum 0 (unsafe-fx+ idx 1)])
       ((= idx len) (new-ISeries v-bop))
       (vector-set! v-bop idx (bop #{(derive-fixnum-value is (vector-ref v1 idx)) : Fixnum} fx))))

; ***********************************************************

; ***********************************************************
; These functions apply addition, subtraction, multiplication
; and division using unsafe-fx and the bop./is function defined
; above.

(: +./is (ISeries Fixnum -> ISeries))
(define (+./is is fx)
  (bop./is fx is unsafe-fx+))

(: -./is (ISeries Fixnum -> ISeries))
(define (-./is is fx)
  (bop./is fx is unsafe-fx-))

(: *./is (ISeries Fixnum -> ISeries))
(define (*./is is fx)
  (bop./is fx is unsafe-fx*))

(: /./is (ISeries Fixnum -> ISeries))
(define (/./is is fx)
  (bop./is fx is unsafe-fxquotient))

(: r./is (ISeries Fixnum -> ISeries))
(define (r./is is fx)
  (bop./is fx is unsafe-fxremainder))

(: %./is (ISeries Fixnum -> ISeries))
(define (%./is is fx)
  (bop./is fx is unsafe-fxmodulo))

; ***********************************************************

; ***********************************************************
;; Binary ISeries comp

; This function consumes 2 integer series and a function which
; consumes 2 Fixnum's and produces a Boolean result for comparison.
; This function is applied to each value in the 2 series at the same
; index resulting in a new boolean point and at the end of the loop
; a new data vector. This data vector is the data of the new ISeries
; which is returned.
(: comp/is (ISeries ISeries (Fixnum Fixnum -> Boolean) -> BSeries))
(define (comp/is is1 is2 comp)
  (define v1 : (Vectorof RFFixnum) (iseries-data (iseries-notna is1)))
  (define v2 : (Vectorof RFFixnum) (iseries-data (iseries-notna is2)))
  (define: len : Index (vector-length v1))
  
  (unless (eqv? len (vector-length v2))
    (error '+/is "Series must be of equal length."))
  
  (define: v-comp : (Vectorof Boolean) (make-vector len #f))

  ; Do loop returns ISeries, idx to 0 and increments by 1 Fixnum on
  ; each iteration (this is the step-exprs). When the loop has gone
  ; through the whole vector, the resulting new ISeries is returned
  ; which the v-bop as the data.
  (do: : BSeries ([idx : Fixnum 0 (unsafe-fx+ idx #{1 : Fixnum})])
       ((= idx len) (new-BSeries v-comp))
       (vector-set! v-comp idx (comp (derive-fixnum-value is1 (vector-ref v1 idx))
                                     (derive-fixnum-value is2 (vector-ref v2 idx))))))

; ***********************************************************

; ***********************************************************
; These functions apply various comparison operators using
; unsafe-fx and the bop./is function defined above.

(: >/is (ISeries ISeries -> BSeries))
(define (>/is is1 is2)
  (comp/is is1 is2 unsafe-fx>))

(: </is (ISeries ISeries -> BSeries))
(define (</is is1 is2)
  (comp/is is1 is2 unsafe-fx<))

(: >=/is (ISeries ISeries -> BSeries))
(define (>=/is is1 is2)
  (comp/is is1 is2 unsafe-fx>=))

(: <=/is (ISeries ISeries -> BSeries))
(define (<=/is is1 is2)
  (comp/is is1 is2 unsafe-fx<=))

(: =/is (ISeries ISeries -> BSeries))
(define (=/is is1 is2)
  (comp/is is1 is2 unsafe-fx=))

(: !=/is (ISeries ISeries -> BSeries))
(define (!=/is is1 is2)
  (comp/is is1 is2 (lambda ([a : Fixnum] [b : Fixnum]) (not (unsafe-fx= a b)))))

; ***********************************************************

; ***********************************************************
;; Scalar ISeries bops

; This function consumes a Fixnum and an integer series and
; a binary operation function which
; consumes 2 Fixnum's and produces a Fixnum result. This function
; is applied to each value in the 2 series at the same index
; resulting in a new data point and at the end of the loop a new
; data vector. This data vector is the data of the new ISeries
; which is returned.

(: comp./is (Fixnum ISeries (Fixnum Fixnum -> Boolean) -> BSeries))
(define (comp./is fx is comp)
  (define: v1 : (Vectorof RFFixnum) (iseries-data (iseries-notna is)))
  (define: len : Index (vector-length v1))
  (define: v-bop : (Vectorof Boolean) (make-vector len #f))

  (do: : BSeries ([idx : Fixnum 0 (unsafe-fx+ idx 1)])
       ((= idx len) (new-BSeries v-bop))

    (if (or (RFNoData? (vector-ref v1 idx)))
        (vector-set! v-bop idx #f)
        (vector-set! v-bop idx (comp #{(derive-fixnum-value is (vector-ref v1 idx)) : Fixnum} fx)))))

; ***********************************************************

; ***********************************************************
; These functions apply comparison operations using unsafe-fx
; and the comp./is function defined above.

(: >./is (ISeries Fixnum -> BSeries))
(define (>./is is fx)
  (comp./is fx is unsafe-fx>))

(: <./is (ISeries Fixnum -> BSeries))
(define (<./is is fx)
  (comp./is fx is unsafe-fx<))

(: >=./is (ISeries Fixnum -> BSeries))
(define (>=./is is fx)
  (comp./is fx is unsafe-fx>=))

(: <=./is (ISeries Fixnum -> BSeries))
(define (<=./is is fx)
  (comp./is fx is unsafe-fx<=))

(: =./is (ISeries Fixnum -> BSeries))
(define (=./is is fx)
  (comp./is fx is unsafe-fx=))

(: !=./is (ISeries Fixnum -> BSeries))
(define (!=./is is fx)
  (comp./is fx is (lambda ([a : Fixnum] [b : Fixnum]) (not (unsafe-fx= a b)))))

; ***********************************************************

; ***********************************************************
(: build-iseries-index-from-predicate (ISeries (RFFixnum -> Boolean) -> RFIndex))
(define (build-iseries-index-from-predicate iseries pred)  
  (build-index-from-list
   (for/list : (Listof IndexDataType)
     ([val (iseries-data iseries)]
      [n (in-naturals)]
      #:when (pred (assert val RFFixnum?)))
     (if (iseries-index iseries)
         (idx->key (assert (iseries-index iseries)) (assert n index?))
         (assert n index?)))))

(: iseries-filter (ISeries (RFFixnum -> Boolean) -> ISeries))
(define (iseries-filter iseries filter-function)
  ; need to use new filtered data to get the new index
  ; setting #f is naive  
  (new-ISeries (vector-filter filter-function (iseries-data iseries)) #:index (build-iseries-index-from-predicate iseries filter-function)))

(: build-iseries-index-from-predicate-not (ISeries (RFFixnum -> Boolean) -> RFIndex))
(define (build-iseries-index-from-predicate-not iseries pred)  
  (build-index-from-list
   (for/list : (Listof IndexDataType)
     ([val (iseries-data iseries)]
      [n (in-naturals)]
      #:when (not (pred (assert val RFFixnum?))))
     (if (iseries-index iseries)
         (idx->key (assert (iseries-index iseries)) (assert n index?))
         (assert n index?)))))

(: iseries-filter-not (ISeries (RFFixnum -> Boolean) -> ISeries))
(define (iseries-filter-not iseries filter-function)
  (new-ISeries (vector-filter-not filter-function (iseries-data iseries)) #:index (build-iseries-index-from-predicate-not iseries filter-function)))
; ***********************************************************

; ***********************************************************
;; ISeries agg ops

; Applies the aggregate function specificed by function-name to the values in
; the column-name column. Currently supports 3: sum, avg, count.
(: apply-agg-is (Symbol ISeries -> GenericType))
(define (apply-agg-is function-name series)
  (let ((data : (Vectorof Fixnum) (vector-map (lambda ([x : RFFixnum]) (if (RFNoData? x) DEFAULT_NULL_VALUE x)) (iseries-data series))))
    (cond 
      [(eq? function-name 'sum) (apply + (vector->list data))]
      [(eq? function-name 'mean) (mean data)]
      [(eq? function-name 'median) (median < data)]
      [(eq? function-name 'mode) (most-frequent-element data)]
      [(eq? function-name 'count) (iseries-length series)]
      [(eq? function-name 'min) (vector-argmin (lambda ([x : Fixnum]) x) data)]
      [(eq? function-name 'max) (vector-argmax (lambda ([x : Fixnum]) x) data)]
      [else (error 'apply-agg-is "Unknown aggregate function.")])))

; ***********************************************************

; ***********************************************************
;; ISeries stat ops

(: apply-stat-is (Symbol ISeries -> Real))
(define (apply-stat-is function-name series)
  (let ((data : (Vectorof Fixnum) (vector-map (lambda ([x : RFFixnum]) (derive-fixnum-value series x)) (iseries-data series))))
    (cond 
      [(eq? function-name 'variance) (variance data)]
      [(eq? function-name 'stddev) (stddev data)]
      [(eq? function-name 'skewness) (skewness data)]
      [else (error 'apply-stat-is "Unknown stat function.")])))

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
(define-predicate ListofFixnum? (Listof Fixnum))
(define-predicate ListofRFFixnum? (Listof RFFixnum))

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

(: iseries-loc-boolean (ISeries (Listof Boolean) -> (U RFFixnum ISeries)))
(define (iseries-loc-boolean iseries boolean-lst)
  (: data (Vectorof RFFixnum))
  (define data (iseries-data iseries))

  (: new-data (Vectorof RFFixnum))
  (define new-data (make-vector (length (filter true? boolean-lst)) 0))
  
  (define data-idx 0)
  (define new-data-idx 0)

  (if (= (length boolean-lst) 1)
      (if (list-ref boolean-lst 0)
          (vector-ref data 0)
          ; empty iseries
          (new-ISeries (vector)))
       
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
      (new-ISeries new-data)))

(: iseries-loc-multi-index (ISeries (U (Listof String) ListofListofString) -> (U RFFixnum ISeries)))
(define (iseries-loc-multi-index iseries label)
  (unless (ISeries-index iseries)
    (let ((k (current-continuation-marks)))
      (raise (make-exn:fail:contract "iseries must have a label index." k))))

  (: get-index-val ((Listof String) -> Symbol))
  (define (get-index-val label)
    (string->symbol (string-append (string-join label key-delimiter) key-delimiter)))
  
  (if (ListofListofString? label)
      (iseries-loc iseries (map get-index-val label))
      (iseries-loc iseries (get-index-val label))))
    
(: iseries-loc (ISeries (U Label (Listof Label) (Listof Boolean)) -> (U RFFixnum ISeries)))
(define (iseries-loc iseries label)
  (unless (ISeries-index iseries)
    (let ((k (current-continuation-marks)))
      (raise (make-exn:fail:contract "iseries must have an index." k))))

  (if (ListofBoolean? label)
      (iseries-loc-boolean iseries label)
      (let ((associated-indices-length : (Listof Integer)
                                       (map (lambda ([l : Label]) (length (iseries-index-ref iseries l))) (convert-to-label-lst label)))
            (vals : (Vectorof RFFixnum)
             (if (list? label)
                 (list->vector (assert (flatten (map (lambda ([l : Label]) (iseries-index-ref iseries l)) label)) ListofRFFixnum?))
                 (list->vector (assert (iseries-index-ref iseries label) ListofRFFixnum?)))))

        (if (= (vector-length vals) 1)
            (vector-ref vals 0)
            (new-ISeries vals #:index (build-index-from-list (build-labels-by-count (convert-to-label-lst label) associated-indices-length)))))))

; vector 0..n index based
(: iseries-iloc (ISeries (U Index (Listof Index)) -> (U RFFixnum ISeries)))
(define (iseries-iloc iseries idx)
  (let ((referencer (iseries-referencer iseries)))
  (if (list? idx)
      ; get labels from SIndex that refer to given indicies
      ; make a new index from these labels using build-index-from-labels
      ; sub-vector the data vector to get the data and create a new-ISeries             
       (if (ISeries-index iseries)           
           (new-ISeries
            (for/vector: : (Vectorof RFFixnum) ([i idx])
              (referencer (assert i index?)))
            #:index (build-index-from-list (map (lambda ([i : Index]) (idx->key (assert (iseries-index iseries)) i)) idx)))
           (new-ISeries
            (for/vector: : (Vectorof RFFixnum) ([i idx])
              (referencer (assert i index?)))))
      (referencer idx))))

(: iseries-iloc-range (ISeries Index Index -> ISeries))
(define (iseries-iloc-range iseries start end)
  ; use vector-copy library method  
   (if (ISeries-index iseries)
       (new-ISeries
        (vector-copy (iseries-data iseries) start end)
       #:index (build-index-from-list (map (lambda ([i : Index]) (idx->key (assert (iseries-index iseries)) i)) (range start end))))
       (new-ISeries
        (vector-copy (iseries-data iseries) start end))))

; ***********************************************************
;; ISeries groupby

(: derive-fixnum-value (ISeries RFFixnum -> Fixnum))
(define (derive-fixnum-value iseries val)
  (if (fixnum? val) (assert val fixnum?) (iseries-null-value iseries)))

(define-type Key String)
(define-type GroupHash (HashTable Key (Listof RFFixnum)))

; This function is self-explanatory, it consumes no arguments
; and creates a hash map which will represent a JoinHash.
(: make-group-hash (-> GroupHash))
(define (make-group-hash)
  (make-hash))

; Used to determine the groups for the groupby. If by is a function, it’s called on each value of the object’s index.
; The Series VALUES will be used to determine the groups if by-value is set to true.
(: iseries-groupby (ISeries [#:by-value Boolean] -> GroupHash))
(define (iseries-groupby iseries #:by-value [by-value #f])
  (define: group-index : GroupHash (make-group-hash))

  (let ((len (iseries-length iseries))
        (k (current-continuation-marks)))
    (if (zero? len)
  (raise (make-exn:fail:contract "iseries can't be empty on groupby." k))
  (begin          
    (do ((i 0 (add1 i)))
        ((>= i len) group-index)
      (let* ((rffixnum-val : (U RFFixnum ISeries) (assert (iseries-iloc iseries (assert i index?)) RFFixnum?))
             (rffixnum-list : (Listof RFFixnum) (if (fixnum? rffixnum-val) (list rffixnum-val) (vector->list (iseries-data iseries))))
                   (key (if by-value
                            (assert (iseries-iloc iseries (assert i index?)) fixnum?)
                            (if (iseries-index iseries)
                                (idx->key (assert (iseries-index iseries)) (assert i index?))
                                (assert i index?))))
                  (key-str : String
                           (cond
                             [(symbol? key) (symbol->string key)]
                             [(number? key) (number->string key)]
                             ; pretty-format anything else
                             [else (pretty-format key)])))              
              (hash-update! group-index key-str
            (λ: ((val : (Listof RFFixnum)))                                
          (append rffixnum-list val))
            (λ () (list)))))))))

;group-hash->series, the keys of hash will be the index
;(: group-hash->series (GroupHash -> ISeries))


; ***********************************************************
;; ISeries agg ops

; Applies the aggregate function specificed by function-name to the values in
; the column-name column. Currently supports 5: sum, mean, median, count.
(: apply-agg-iseries (Symbol GroupHash -> GenSeries))
(define (apply-agg-iseries function-name group-hash)
  (define len (hash-count group-hash))

  (: agg-value-hash AggValueHash)
  (define agg-value-hash (make-hash))

  (hash-for-each group-hash
                 (lambda ([key : String] [val : (Listof RFFixnum)])
                   
                   (let ((key (assert key string?))
                         (val (assert (flatten (andmap fixnum? val)) ListofFixnum?)))
                     (hash-set! agg-value-hash key
                                (cond 
                                  [(eq? function-name 'sum) (apply + val)]
                                  [(eq? function-name 'mean) (mean val)]
                                  [(eq? function-name 'median) (median (lambda ([val1 : Fixnum] [val2 : Fixnum]) (< val1 val2)) val)]
                                  [(eq? function-name 'mode) (most-frequent-element val)]
                                  [(eq? function-name 'count) (length val)]
                                  [(eq? function-name 'min) (argmin (lambda ([x : Real]) x) val)]
                                  [(eq? function-name 'max) (argmax (lambda ([x : Real]) x) val)]
                                  [else (error 'apply-agg-data-frame "Unknown aggregate function.")])))))

  (agg-value-hash-to-gen-series agg-value-hash))

(: iseries->gen-series (ISeries -> GenSeries))
(define (iseries->gen-series iseries)
  (let* ((data (iseries-data iseries))
         (len (vector-length data)))
    
     (let: ((new-data : (Vectorof GenericType) ((inst make-vector GenericType) len 0)))
       (begin
         (do ([idx 0 (add1 idx)])
           ([>= idx len] new-data)
           (vector-set! new-data idx (vector-ref data idx)))
         (new-GenSeries new-data #:index (iseries-index iseries) #:fill-null (iseries-null-value iseries))))))

; ***********************************************************
; ISeries Nominals
; ***********************************************************
(struct: ISeries-Nominals
  ([index : (Option RFIndex)]
   [data : (Vectorof Index)]
   [nominals : (Vectorof RFFixnum)]   
   [null-value : RFNULL]
   [fixnum-null-value : Fixnum])
  #:mutable
  #:transparent)

(: new-ISeries-Nominals ((Vectorof RFFixnum) (Option (U (Listof IndexDataType) RFIndex)) [#:fill-null RFNULL] -> ISeries-Nominals))
(define (new-ISeries-Nominals nominals labels #:fill-null [null-value DEFAULT_NULL_VALUE])

  (: nominal-code (HashTable RFFixnum Index))
  (define nominal-code (make-hash))

  (define fixnum-null-value : Fixnum (if (fixnum? null-value) null-value DEFAULT_NULL_VALUE))

  (define len (vector-length nominals))

  (: data (Vectorof Index))
  (define data (make-vector len 0))

  (: make-nominal-vector (-> (Vectorof RFFixnum)))
  (define (make-nominal-vector)
    (define nominals : (Vectorof RFFixnum) (make-vector (hash-count nominal-code) DEFAULT_NULL_VALUE))
    (hash-for-each nominal-code
		   (λ: ((nom : RFFixnum) (idx : Index))
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
  
  (let: loop : ISeries-Nominals ((idx : Natural 0) (code : Index 0))
    (if (>= idx len)
        (if (RFIndex? labels)
            (begin
              (check-mismatch labels)
              (ISeries-Nominals labels data (make-nominal-vector) null-value fixnum-null-value))
            (ISeries-Nominals #f data (make-nominal-vector) null-value fixnum-null-value))
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
(: set-ISeries-Nominals-index (ISeries-Nominals (U (Listof IndexDataType) RFIndex) -> ISeries-Nominals))
(define (set-ISeries-Nominals-index iseries-nominals labels)
  (new-ISeries-Nominals (ISeries-Nominals-nominals iseries-nominals) labels))

(: set-ISeries-Nominals-null-value (ISeries-Nominals RFNULL -> ISeries-Nominals))
(define (set-ISeries-Nominals-null-value iseries-nominals null-value)
  (new-ISeries-Nominals (iseries-nominals-data iseries-nominals) (iseries-nominals-index iseries-nominals) #:fill-null null-value))

(: set-ISeries-Nominals-fixnum-null-value-inplace (ISeries-Nominals Fixnum -> Void))
(define (set-ISeries-Nominals-fixnum-null-value-inplace iseries-nominals null-value)
  (set-ISeries-Nominals-fixnum-null-value! iseries-nominals null-value))
; ***********************************************************

(: iseries-nominals-referencer (ISeries-Nominals -> (Fixnum -> RFFixnum)))
(define (iseries-nominals-referencer iseries-nominals)
  (let ((data (ISeries-Nominals-data iseries-nominals))
	(noms (ISeries-Nominals-nominals iseries-nominals)))
    (λ: ((idx : Fixnum))
	(let ((code (vector-ref data idx)))
	  (vector-ref noms code)))))

(: iseries-nominals-iref (ISeries-Nominals (Listof Index) -> (Listof RFFixnum)))
(define (iseries-nominals-iref iseries-nominals lst-idx)
  (map (lambda ((idx : Index))
         (vector-ref (ISeries-Nominals-nominals iseries-nominals)
                     (vector-ref (ISeries-Nominals-data iseries-nominals) idx)))
       lst-idx))

; This function consumes an integer series and an index and
; returns a vector of values in the range [0:index] in the series.
(: iseries-nominals-range (ISeries-Nominals Index -> (Vectorof RFFixnum)))
(define (iseries-nominals-range series pos)
  (vector-map (lambda ([idx : Index]) (vector-ref (ISeries-Nominals-nominals series) idx))
              (vector-take (ISeries-Nominals-data series) pos)))

(: iseries-nominals-length (ISeries-Nominals -> Index))
(define (iseries-nominals-length series)
  (vector-length (ISeries-Nominals-data series)))

(: iseries-nominals-nominal-data (ISeries-Nominals -> (Vectorof RFFixnum)))
(define (iseries-nominals-nominal-data series)
  (ISeries-Nominals-nominals series))

(: iseries-nominals-ref-idx-data (ISeries-Nominals -> (Vectorof Index)))
(define (iseries-nominals-ref-idx-data series)
  (ISeries-Nominals-data series))

(: iseries-nominals-data (ISeries-Nominals -> (Vectorof RFFixnum)))
(define (iseries-nominals-data series)
  (vector-map (lambda ([i : Index]) (car (iseries-nominals-iref series (list i)))) (ISeries-Nominals-data series)))

(: iseries-nominals-index (ISeries-Nominals -> (U False RFIndex)))
(define (iseries-nominals-index series)
  (ISeries-Nominals-index series))

; This function consumes an integer series and returns its
; data vector.
(: iseries-nominals-custom-null-value (ISeries-Nominals -> RFNULL))
(define (iseries-nominals-custom-null-value iseries-nominals)
  (ISeries-Nominals-null-value iseries-nominals))

; This function consumes an integer series and returns its
; fixnum null value.
(: iseries-nominals-null-value (ISeries-Nominals -> RFFixnum))
(define (iseries-nominals-null-value iseries-nominals)
  (ISeries-Nominals-fixnum-null-value iseries-nominals))

; This function consumes an integer series and a integer value
; and returns whether it is considered to be NULL in the series.
(: iseries-nominals-value-is-null? (ISeries-Nominals RFFixnum -> Boolean))
(define (iseries-nominals-value-is-null? iseries-nominals value)
  (eq? (iseries-nominals-null-value iseries-nominals) value))

; This function consumes a series and a RFFixnum and returns
; the list of values at that RFFixnum in the series.
(: iseries-nominals-index-ref (ISeries-Nominals IndexDataType -> (Listof RFFixnum)))
(define (iseries-nominals-index-ref series item)
  (iseries-nominals-iref series (key->lst-idx (assert (ISeries-Nominals-index series)) item)))

; label based
; for two different use cases:
; a.) Selecting rows by label/index
; b.) Selecting rows with a boolean / conditional lookup

(: iseries-nominals-loc-multi-index (ISeries-Nominals (U (Listof String) ListofListofString) -> (U RFFixnum ISeries-Nominals)))
(define (iseries-nominals-loc-multi-index iseries-nominals label)
  (unless (ISeries-Nominals-index iseries-nominals)
    (let ((k (current-continuation-marks)))
      (raise (make-exn:fail:contract "iseries-nominals must have a label index." k))))

  (: get-index-val ((Listof String) -> Label))
  (define (get-index-val label)
    (string->symbol (string-append (string-join label key-delimiter) key-delimiter)))
  
  (if (ListofListofString? label)
      (iseries-nominals-loc iseries-nominals (map get-index-val label))
      (iseries-nominals-loc iseries-nominals (get-index-val label))))

(: iseries-nominals-loc (ISeries-Nominals (U Label (Listof Label) (Listof Boolean)) -> (U RFFixnum ISeries-Nominals)))
(define (iseries-nominals-loc iseries-nominals label)
  (unless (ISeries-Nominals-index iseries-nominals)
    (let ((k (current-continuation-marks)))
      (raise (make-exn:fail:contract "iseries must have an index." k))))

  (if (ListofBoolean? label)
      (iseries-nominals-loc-boolean iseries-nominals label)
      (let ((associated-indices-length : (Listof Integer)
                                       (map (lambda ([l : Label]) (length (iseries-nominals-index-ref iseries-nominals l))) (convert-to-label-lst label)))
            (vals : (Vectorof RFFixnum)
             (if (list? label)
                 (list->vector (assert (flatten (map (lambda ([l : Label]) (iseries-nominals-index-ref iseries-nominals l)) label)) ListofRFFixnum?))
                 (list->vector (assert (iseries-nominals-index-ref iseries-nominals label) ListofRFFixnum?)))))

        (if (= (vector-length vals) 1)
            (vector-ref vals 0)
            (new-ISeries-Nominals vals (build-index-from-list (build-labels-by-count (convert-to-label-lst label) associated-indices-length)))))))    

; index based
(: iseries-nominals-iloc (ISeries-Nominals (U Index (Listof Index)) -> (U RFFixnum ISeries-Nominals)))
(define (iseries-nominals-iloc iseries-nominals idx)
  (let ((referencer (iseries-nominals-referencer iseries-nominals)))
  (if (list? idx)
      ; get labels from SIndex that refer to given indicies
      ; make a new index from these labels using build-index-from-labels
      ; sub-vector the data vector to get the data and create a new-BSeries
      (new-ISeries-Nominals
       (for/vector: : (Vectorof RFFixnum) ([i idx])
         (vector-ref (iseries-nominals-data iseries-nominals) i))
       
       (if (not (ISeries-Nominals-index iseries-nominals))
           (build-index-from-list (range (length idx)))
           (build-index-from-list (map (lambda ([i : Index]) (idx->key (assert (ISeries-Nominals-index iseries-nominals)) i)) idx))))
      (referencer idx))))

(: iseries-nominals-loc-boolean (ISeries-Nominals (Listof Boolean) -> (U RFFixnum ISeries-Nominals)))
(define (iseries-nominals-loc-boolean iseries-nominals boolean-lst)
  (: data (Vectorof RFFixnum))
  (define data (iseries-nominals-data iseries-nominals))

  (: new-data (Vectorof RFFixnum))
  (define new-data (make-vector (length (filter true? boolean-lst)) DEFAULT_NULL_VALUE))
  
  (define data-idx 0)
  (define new-data-idx 0)

  (if (= (length boolean-lst) 1)
      (if (list-ref boolean-lst 0)
          (vector-ref data 0)
          ; empty iseries-nominals
          (new-ISeries-Nominals (vector) #f))
       
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
      (new-ISeries-Nominals new-data #f)))

; Used to determine the groups for the groupby on each value of the series index by default.
; The Series VALUES will be used to determine the groups if by-value is set to true.
(: iseries-nominals-groupby (ISeries-Nominals [#:by-value Boolean] -> GroupHash))
(define (iseries-nominals-groupby iseries-nominals #:by-value [by-value #f])
  (define: group-index : GroupHash (make-group-hash))

  (let ((len (iseries-nominals-length iseries-nominals))
        (k (current-continuation-marks)))
    (if (zero? len)
	(raise (make-exn:fail:contract "iseries-nominals can't be empty on groupby." k))
	(begin          
	  (do ((i 0 (add1 i)))
	      ((>= i len) group-index)
	    (let* ((label-val : (U RFFixnum ISeries-Nominals) (iseries-nominals-iloc iseries-nominals (assert i index?)))
                   (label-list : (Listof RFFixnum) (if (RFFixnum? label-val) (list label-val) (vector->list (iseries-nominals-data label-val))))
                   (key (if by-value
                            (iseries-nominals-iloc iseries-nominals (assert i index?))
                            (if (iseries-nominals-index iseries-nominals)
                                (idx->key (assert (iseries-nominals-index iseries-nominals)) (assert i index?))
                                (assert i index?))))
                  (key-str : String (cond
                                      [(symbol? key) (symbol->string key)]
                                      [(number? key) (number->string key)]
                                      ; pretty-format anything else
                                      [else (pretty-format key)])))              
              (hash-update! group-index key-str
			      (λ: ((val : (Listof RFFixnum)))                                
				  (append label-list val))
			      (λ () (list)))))))))

; ***********************************************************
; ISeries Nominals
; ***********************************************************
