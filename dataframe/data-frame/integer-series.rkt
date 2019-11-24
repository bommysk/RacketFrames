;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*    Author: Shubham Kahal
;*    File: integer-series.rkt
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
 (struct-out ISeries)
 ISeries-index)

(provide:
 [new-ISeries ((Vectorof Fixnum) (Option (U (Listof IndexDataType) RFIndex)) -> ISeries)]
 [set-ISeries-index (ISeries (U (Listof Label) RFIndex) -> ISeries)]
 [iseries-iref (ISeries (Listof Index) -> (Listof Fixnum))]
 [iseries-loc-boolean (ISeries (Listof Boolean) -> (U Fixnum ISeries))]
 [iseries-loc (ISeries (U Label (Listof Label) (Listof Boolean)) -> (U Fixnum ISeries))]
 [iseries-iloc (ISeries (U Index (Listof Index)) -> (U Fixnum ISeries))]
 [iseries-index-ref (ISeries IndexDataType -> (Listof Integer))]
 [iseries-range (ISeries Index -> (Vectorof Fixnum))]
 [iseries-length (ISeries -> Index)]
 [iseries-referencer (ISeries -> (Index -> Fixnum))]
 [iseries-data (ISeries -> (Vectorof Fixnum))]
 [iseries-index (ISeries -> (U False RFIndex))]
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
 [apply-agg-is (Symbol ISeries -> Real)]
 [apply-stat-is (Symbol ISeries -> Real)]
 [iseries-print (ISeries Output-Port -> Void)]
 [iseries-filter (ISeries (Fixnum -> Boolean) -> ISeries)]
 [iseries-filter-not (ISeries (Fixnum -> Boolean) -> ISeries)])
; ***********************************************************

; ***********************************************************
; use build-index-from-labels function and Label, SIndex and
; LabelIndex structs from indexed-series.
(require
 racket/unsafe/ops
 (only-in "indexed-series.rkt"
	  build-index-from-list          
          RFIndex RFIndex? IndexDataType
          extract-index
	  Label SIndex LabelIndex LabelIndex-index
          FIndex FloatIndex
          label-index key->lst-idx
          idx->key is-labeled? ListofIndexDataType?
          is-indexed?)
 (only-in "boolean-series.rkt"
          BSeries))
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

;; Integer series optimized with use of Fixnum.
(struct ISeries ([index : (Option RFIndex)] [data : (Vectorof Fixnum)]))

; Consumes a Vector of Fixnum and a list of Labels which
; can come in list form or SIndex form and produces a ISeries
; struct object.
(: new-ISeries ((Vectorof Fixnum) (Option (U (Listof IndexDataType) RFIndex)) -> ISeries))
(define (new-ISeries data labels)

  (: check-mismatch (RFIndex -> Void))
  (define (check-mismatch index)
    ; bug spotted, needs to count sum of the number of elements in each hashed list
    (unless (eq? (vector-length data) (hash-count (extract-index index)))
      (let ((k (current-continuation-marks)))
	(raise (make-exn:fail:contract "Cardinality of a Series' data and labels must be equal" k))))
    (void))

  (if (RFIndex? labels)
      (begin
	(check-mismatch labels)
	(ISeries labels data))
      (if labels
	  (let ((index (build-index-from-list (assert labels ListofIndexDataType?))))
	    (check-mismatch index)
	    (ISeries index data))
	  (ISeries #f data))))
; ***********************************************************

; ***********************************************************
(: set-ISeries-index (ISeries (U (Listof IndexDataType) RFIndex) -> ISeries))
(define (set-ISeries-index iseries labels)
  (new-ISeries (iseries-data iseries) labels))
; ***********************************************************

; ***********************************************************
(: iseries-print (ISeries Output-Port -> Void))
(define (iseries-print iseries port)
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
	    (let ((num (vector-ref v i)))
              (if (ISeries-index iseries)
                  (display (idx->key (ISeries-index iseries) (assert i index?)) port)
                  (display (assert i index?) port))
              (display " " port)
              (displayln num port)))))))
; ***********************************************************

; ***********************************************************
; This function consumes an integer series and returns a
; lambda function which consumes an index and provides the
; value of the data at that index in the series. It can be
; defined once and used repeatedly as a referencer.
(: iseries-referencer (ISeries -> (Index -> Fixnum)))
(define (iseries-referencer iseries)
  (let ((data (ISeries-data iseries)))
    (λ: ((idx : Index))
	(vector-ref data idx))))

; This function consumes an integer series and an index and
; returns the value at that index in the series.
(: iseries-iref (ISeries (Listof Index) -> (Listof Fixnum)))
(define (iseries-iref series lst-idx)
  (map (lambda ((idx : Index)) (vector-ref (ISeries-data series) idx))
       lst-idx))

; This function consumes an integer series and an index and
; returns a vector of values in the range [0:index] in the series.
(: iseries-range (ISeries Index -> (Vectorof Fixnum)))
(define (iseries-range series pos)
   (vector-take (ISeries-data series) pos))

; This function consumes an integer series and returns its
; data vector.
(: iseries-data (ISeries -> (Vectorof Fixnum)))
(define (iseries-data series)
  (ISeries-data series))

; This function consumes an integer series and returns its
; data vector.
(: iseries-index (ISeries -> (U False RFIndex)))
(define (iseries-index series)
  (ISeries-index series))

; This function consumes a series and a Label and returns
; the list of values at that Label in the series.
(: iseries-index-ref (ISeries IndexDataType -> (Listof Integer)))
(define (iseries-index-ref series item)
  (iseries-iref series (key->lst-idx (assert (ISeries-index series)) item)))

; This function consumes an integer series and returns the
; length of that series.
(: iseries-length (ISeries -> Index))
(define (iseries-length series)
  (vector-length (ISeries-data series)))
; ***********************************************************

; ***********************************************************
(: map/is (ISeries (Fixnum -> Fixnum) -> ISeries))
(define (map/is series fn)
  (let ((old-data (ISeries-data series)))
    (ISeries #f (build-vector (vector-length old-data)
                              (λ: ((idx : Natural))
                                (fn (vector-ref old-data idx)))))))
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
(define (bop/is ns1 ns2 bop)
  (define v1 (ISeries-data ns1))
  (define v2 (ISeries-data ns2))
  (define: len : Index (vector-length v1))
  
  (unless (eqv? len (vector-length v2))
	  (error '+/is "Series must be of equal length."))
  
  (define: v-bop : (Vectorof Fixnum) (make-vector len #{0 : Fixnum}))

  ; Do loop returns ISeries, idx to 0 and increments by 1 Fixnum on
  ; each iteration (this is the step-exprs). When the loop has gone
  ; through the whole vector, the resulting new ISeries is returned
  ; which the v-bop as the data.
  (do: : ISeries ([idx : Fixnum 0 (unsafe-fx+ idx #{1 : Fixnum})])
       ((= idx len) (ISeries #f v-bop))
       (vector-set! v-bop idx (bop (vector-ref v1 idx)
				   (vector-ref v2 idx)))))
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
(define (+/is ns1 ns2)
  (bop/is ns1 ns2 unsafe-fx+))

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
  (define: v1 : (Vectorof Fixnum) (ISeries-data is))
  (define: len : Index (vector-length v1))
  (define: v-bop : (Vectorof Fixnum) ((inst make-vector Fixnum) len #{0 : Fixnum}))

  (do: : ISeries ([idx : Fixnum 0 (unsafe-fx+ idx 1)])
       ((= idx len) (ISeries #f v-bop))
       (vector-set! v-bop idx (bop #{(vector-ref v1 idx) : Fixnum} fx))))

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
(define (comp/is ns1 ns2 comp)
  (define v1 (ISeries-data ns1))
  (define v2 (ISeries-data ns2))
  (define: len : Index (vector-length v1))
  
  (unless (eqv? len (vector-length v2))
	  (error '+/is "Series must be of equal length."))
  
  (define: v-comp : (Vectorof Boolean) (make-vector len #f))

  ; Do loop returns ISeries, idx to 0 and increments by 1 Fixnum on
  ; each iteration (this is the step-exprs). When the loop has gone
  ; through the whole vector, the resulting new ISeries is returned
  ; which the v-bop as the data.
  (do: : BSeries ([idx : Fixnum 0 (unsafe-fx+ idx #{1 : Fixnum})])
       ((= idx len) (BSeries #f v-comp))
       (vector-set! v-comp idx (comp (vector-ref v1 idx)
				   (vector-ref v2 idx)))))

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
  (define: v1 : (Vectorof Fixnum) (ISeries-data is))
  (define: len : Index (vector-length v1))
  (define: v-bop : (Vectorof Boolean) (make-vector len #f))

  (do: : BSeries ([idx : Fixnum 0 (unsafe-fx+ idx 1)])
       ((= idx len) (BSeries #f v-bop))
       (vector-set! v-bop idx (comp #{(vector-ref v1 idx) : Fixnum} fx))))

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
(: iseries-filter (ISeries (Fixnum -> Boolean) -> ISeries))
(define (iseries-filter iseries filter-function)
  (ISeries #f (vector-filter filter-function (ISeries-data iseries))))

(: iseries-filter-not (ISeries (Fixnum -> Boolean) -> ISeries))
(define (iseries-filter-not iseries filter-function)
  (ISeries #f (vector-filter-not filter-function (ISeries-data iseries))))
; ***********************************************************

; ***********************************************************
;; ISeries agg ops

; Applies the aggregate function specificed by function-name to the values in
; the column-name column. Currently supports 3: sum, avg, count.
(: apply-agg-is (Symbol ISeries -> Real))
(define (apply-agg-is function-name series)
  (cond 
    [(eq? function-name 'sum) (apply + (vector->list (ISeries-data series)))]
    [(eq? function-name 'mean) (mean (vector->list (ISeries-data series)))]
    ;[(eq? function-name 'median) (median (vector->list (ISeries-data series)))]
    ;[(eq? function-name 'mode) (mode (vector->list (ISeries-data series)))]
    [(eq? function-name 'count) (iseries-length series)]
    [(eq? function-name 'min) (vector-argmin (lambda ([x : Fixnum]) x) (ISeries-data series))]
    [(eq? function-name 'max) (vector-argmax (lambda ([x : Fixnum]) x) (ISeries-data series))]
    [else (error 'apply-agg-is "Unknown aggregate function.")]))

; ***********************************************************

; ***********************************************************
;; ISeries stat ops

(: apply-stat-is (Symbol ISeries -> Real))
(define (apply-stat-is function-name series)
  (cond 
    [(eq? function-name 'variance) (variance (vector->list (ISeries-data series)))]
    [(eq? function-name 'stddev) (stddev (vector->list (ISeries-data series)))]
    [(eq? function-name 'skewness) (skewness (vector->list (ISeries-data series)))]
    [else (error 'apply-stat-is "Unknown stat function.")]))

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

(: iseries-loc-boolean (ISeries (Listof Boolean) -> (U Fixnum ISeries)))
(define (iseries-loc-boolean iseries boolean-lst)
  (: data (Vectorof Fixnum))
  (define data (iseries-data iseries))

  (: new-data (Vectorof Fixnum))
  (define new-data (make-vector (length (filter true? boolean-lst)) 0))
  
  (define data-idx 0)
  (define new-data-idx 0)

  (if (= (length boolean-lst) 1)
      (if (list-ref boolean-lst 0)
          (vector-ref data 0)
          ; empty iseries
          (new-ISeries (vector) #f))
       
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
      (new-ISeries new-data #f)))
    
(: iseries-loc (ISeries (U Label (Listof Label) (Listof Boolean)) -> (U Fixnum ISeries)))
(define (iseries-loc iseries label)
  (unless (ISeries-index iseries)
    (let ((k (current-continuation-marks)))
      (raise (make-exn:fail:contract "iseries must have an index." k))))

  (if (ListofBoolean? label)
      (iseries-loc-boolean iseries label)
      (let ((associated-indices-length : (Listof Integer)
                                       (map (lambda ([l : Label]) (length (iseries-index-ref iseries l))) (convert-to-label-lst label)))
            (vals : (Vectorof Fixnum)
             (if (list? label)
                 (list->vector (assert (flatten (map (lambda ([l : Label]) (iseries-index-ref iseries l)) label)) ListofFixnum?))
                 (list->vector (assert (iseries-index-ref iseries label) ListofFixnum?)))))

        (if (= (vector-length vals) 1)
            (vector-ref vals 0)
            (new-ISeries vals (build-index-from-list (build-labels-by-count (convert-to-label-lst label) associated-indices-length)))))))

; index based
(: iseries-iloc (ISeries (U Index (Listof Index)) -> (U Fixnum ISeries)))
(define (iseries-iloc iseries idx)
  (let ((referencer (iseries-referencer iseries)))
  (if (list? idx)
      ; get labels from SIndex that refer to given indicies
      ; make a new index from these labels using build-index-from-labels
      ; sub-vector the data vector to get the data and create a new-ISeries
      (new-ISeries
       (for/vector: : (Vectorof Fixnum) ([i idx])
         (vector-ref (iseries-data iseries) i))
       (build-index-from-list (map (lambda ([i : Index]) (idx->key (assert (ISeries-index iseries)) i)) idx)))
      (referencer idx))))

; ***********************************************************