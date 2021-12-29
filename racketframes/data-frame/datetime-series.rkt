;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*    Author: Shubham Kahal
;*    File: datetime-indexed-series.rkt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket

(require typed/racket/date)

(require
 (only-in "../util/datetime.rkt"
          Datetime Datetime? Date Time datetime-date datetime-time
          rf-date-year rf-date-month rf-date-day
          time-offset time-hour time-minute time-second time-milli
          datetime->string)
 racket/unsafe/ops
 (only-in "indexed-series.rkt"
	  RFIndex RFIndex? build-index-from-list
          IndexDataType extract-index
          Label LabelIndex-index
          LabelIndex label-index label->lst-idx key->lst-idx
          idx->key is-indexed? ListofIndexDataType? ListofIndex?
          ListofBoolean? ListofFixnum?
          ListofListofString ListofListofString? RFNoData RFNULL)
 (only-in "boolean-series.rkt"
          new-BSeries BSeries))

;(date->seconds (date 1 2 3 4 5 1971 5 8 #f 0))

; ***********************************************************
; Provide functions in this file to other files.
(provide
 (struct-out DatetimeSeries) RFDatetime RFDatetime? (rename-out [DEFAULT_NULL_VALUE DATETIME_SERIES_DEFAULT_NULL_VALUE] [GroupHash datetime-series-grouphash]))

(provide:
 [new-DatetimeSeries ((U (Vectorof Datetime) (Sequenceof Datetime) (Sequenceof RFDatetime))
                      [#:index (Option (U (Sequenceof IndexDataType) RFIndex))] [#:fill-null RFNULL] -> DatetimeSeries)]
 [set-DatetimeSeries-index (DatetimeSeries (U (Listof IndexDataType) RFIndex) -> DatetimeSeries)] 
 [datetime-series-iref (DatetimeSeries (Listof Index) -> (Listof RFDatetime))]
 [datetime-series-index-ref (DatetimeSeries IndexDataType -> (Listof RFDatetime))]
 [datetime-series-loc-multi-index (DatetimeSeries (U (Listof String) ListofListofString) -> (U RFDatetime DatetimeSeries))]
 [datetime-series-loc-boolean (DatetimeSeries (Listof Boolean) -> (U RFDatetime DatetimeSeries))]
 [datetime-series-loc (DatetimeSeries (U Label (Listof Label) (Listof Boolean)) -> (U RFDatetime DatetimeSeries))]
 [datetime-series-iloc (DatetimeSeries (U Index (Listof Index)) -> (U RFDatetime DatetimeSeries))]
 [datetime-series-iloc-range (DatetimeSeries Index Index -> DatetimeSeries)]
 [datetime-series-label-ref (DatetimeSeries Label -> (Listof RFDatetime))]
 [datetime-series-range (DatetimeSeries Index Index -> (Vectorof RFDatetime))]
 [datetime-series-length (DatetimeSeries -> Index)]
 [datetime-series-referencer (DatetimeSeries -> (Index -> RFDatetime))]
 [datetime-series-data (DatetimeSeries -> (Vectorof RFDatetime))]
 [datetime-series-index (DatetimeSeries -> (U False RFIndex))]
 [datetime-series-null-value (DatetimeSeries -> RFNULL)]
 [datetime-series-datetime-null-value (DatetimeSeries -> Datetime)]
 [in-datetime-series (Datetime DatetimeSeries -> Boolean)]
 [map/datetime-series-data (DatetimeSeries (Datetime -> Datetime) -> DatetimeSeries)]
 [datetime-range (Datetime (Option Symbol) (Option Index) (Option Datetime) -> (Listof RFDatetime))]

 [bop/datetime-series (DatetimeSeries DatetimeSeries (Datetime Datetime -> Datetime) -> DatetimeSeries)]
 [comp/datetime-series (DatetimeSeries DatetimeSeries (Datetime Datetime -> Boolean) -> BSeries)]
 [+/datetime-series (DatetimeSeries DatetimeSeries -> DatetimeSeries)]
 [-/datetime-series (DatetimeSeries DatetimeSeries -> DatetimeSeries)]
 ;[+./datetime-series (DatetimeSeries Datetime -> DatetimeSeries)]
 ;[-./datetime-series (DatetimeSeries Datetime -> DatetimeSeries)]
 [>/datetime-series (DatetimeSeries DatetimeSeries -> BSeries)]
 [</datetime-series (DatetimeSeries DatetimeSeries -> BSeries)]
 [>=/datetime-series (DatetimeSeries DatetimeSeries -> BSeries)]
 [<=/datetime-series (DatetimeSeries DatetimeSeries -> BSeries)]
 [=/datetime-series (DatetimeSeries DatetimeSeries -> BSeries)]
 [!=/datetime-series (DatetimeSeries DatetimeSeries -> BSeries)]
 [datetime-series-print (DatetimeSeries [#:output-port Output-Port] [#:count (Option Index)] -> Void)]
 [datetime-series-groupby (DatetimeSeries [#:by-value Boolean] -> GroupHash)]
 [set-DatetimeSeries-null-value (DatetimeSeries RFNULL -> DatetimeSeries)]
 [set-DatetimeSeries-datetime-null-value-inplace (DatetimeSeries Datetime -> Void)]
 [datetime-series-index-from-predicate (DatetimeSeries (RFDatetime -> Boolean) -> RFIndex)]
 [datetime-series-index-from-predicate-not (DatetimeSeries (RFDatetime -> Boolean) -> RFIndex)]
 [datetime-series-data-idxes-from-predicate (DatetimeSeries (RFDatetime -> Boolean) -> (Listof Index))]
 [datetime-series-data-idxes-from-predicate-not (DatetimeSeries (RFDatetime -> Boolean) -> (Listof Index))]
 [datetime-series-filter (DatetimeSeries (RFDatetime -> Boolean) -> DatetimeSeries)]
 [datetime-series-filter-not (DatetimeSeries (RFDatetime -> Boolean) -> DatetimeSeries)]
 [datetime-series-sort (DatetimeSeries -> DatetimeSeries)]
 [datetime-series-sort-descending (DatetimeSeries -> DatetimeSeries)])
; ***********************************************************

(define-type RFDatetime (U Datetime RFNoData))
(define-predicate RFDatetime? RFDatetime)
(define DEFAULT_NULL_VALUE : Datetime (Datetime (Date 0 0 0) (Time 0 0 0 0 0)))
(struct DatetimeSeries
  ([index : (Option RFIndex)]
   [data : (Vectorof RFDatetime)]
   ; when the null-value is not a fixnum?, the fixnum-null-value is set to 0
   [null-value : RFNULL]
   ; needed for type checking purposes and to do proper arithmetic operations in numeric series
   [datetime-null-value : Datetime])
  #:mutable
  #:transparent)

(: make-RFDatetime-vector ((U (Sequenceof Datetime) (Sequenceof RFDatetime)) -> (Vectorof RFDatetime)))
(define (make-RFDatetime-vector seq)
  (let ((vec : (Vectorof RFDatetime) (list->vector (sequence->list seq))))
    vec))

; Consumes a Vector of Fixnum and a list of Labels which
; can come in list form or SIndex form and produces a DatetimeSeries
; struct object.
(: new-DatetimeSeries ((U (Vectorof Datetime) (Sequenceof Datetime) (Sequenceof RFDatetime))
                       [#:index (Option (U (Sequenceof IndexDataType) RFIndex))] [#:fill-null RFNULL] -> DatetimeSeries))
(define (new-DatetimeSeries data #:index [labels #f] #:fill-null [null-value DEFAULT_NULL_VALUE])

  (define data-vector : (Vectorof RFDatetime) (make-RFDatetime-vector data))

  (: check-mismatch (RFIndex -> Void))
  (define (check-mismatch index)    
    (let ((index-length (apply + (for/list: : (Listof Index)
                                   ([value (in-hash-values (extract-index index))])
                                   (length (assert value ListofIndex?))))))

      (unless (eq? (vector-length data-vector) index-length)
        (let ((k (current-continuation-marks)))
          (raise (make-exn:fail:contract "Cardinality of a Series' data and labels must be equal" k))))
      (void)))

  (define dt-null-value : Datetime (if (Datetime? null-value) null-value DEFAULT_NULL_VALUE))
  
  (if (RFIndex? labels)
      (begin
	(check-mismatch (assert labels))
	(DatetimeSeries labels data-vector null-value dt-null-value))
      (if labels
	  (let ((index (build-index-from-list (assert labels ListofIndexDataType?))))
	    (check-mismatch index)
	    (DatetimeSeries index data-vector null-value dt-null-value))
	  (DatetimeSeries #f data-vector null-value dt-null-value))))
; ***********************************************************

; ***********************************************************
(: set-DatetimeSeries-index (DatetimeSeries (U (Listof IndexDataType) RFIndex) -> DatetimeSeries))
(define (set-DatetimeSeries-index datetime-series labels)
  (new-DatetimeSeries (datetime-series-data datetime-series) #:index labels))

(: set-DatetimeSeries-null-value (DatetimeSeries RFNULL -> DatetimeSeries))
(define (set-DatetimeSeries-null-value datetime-series null-value)
  (new-DatetimeSeries (datetime-series-data datetime-series) #:index (datetime-series-index datetime-series) #:fill-null null-value))

(: set-DatetimeSeries-datetime-null-value-inplace (DatetimeSeries Datetime -> Void))
(define (set-DatetimeSeries-datetime-null-value-inplace datetime-series null-value)
  (set-DatetimeSeries-datetime-null-value! datetime-series null-value))
; ***********************************************************

; ***********************************************************
(: datetime-series-print (DatetimeSeries [#:output-port Output-Port] [#:count (Option Index)] -> Void))
(define (datetime-series-print datetime-series #:output-port [port (current-output-port)] #:count [count #f])
  (define date-v (datetime-series-data datetime-series))
  (define v (datetime-series-data datetime-series))
  (let ((len (if (assert count) count (vector-length v)))
	(out (current-output-port)))
    (if (zero? len)
	(displayln "Empty $DatetimeSeries" port)
	(begin
          (displayln "*********" port)
          (displayln "$DatetimeSeries" port)
          (displayln "*********" port)
	  (do ((i 0 (add1 i)))
	      ((>= i len) (void))
	    (let ((date (vector-ref date-v i))
                  (num (vector-ref v i)))
              (if (DatetimeSeries-index datetime-series)                  
                  (display (idx->key (assert (DatetimeSeries-index datetime-series)) (assert i index?)) port)
                  (display (assert i index?) port))
              (display " " port)
              (displayln date port)
              (displayln num port)))))))
; ***********************************************************

; ***********************************************************
; This function consumes an integer series and returns a
; lambda function which consumes an index and provides the
; value of the data at that index in the series. It can be
; defined once and used repeatedly as a referencer.
(: datetime-series-referencer (DatetimeSeries -> (Index -> RFDatetime)))
(define (datetime-series-referencer datetime-series)
  (let ((data (DatetimeSeries-data datetime-series)))
    (λ: ((idx : Index))
	(vector-ref data idx))))

; This function consumes an integer series and an index and
; returns the value at that index in the series.
(: datetime-series-iref (DatetimeSeries (Listof Index) -> (Listof RFDatetime)))
(define (datetime-series-iref datetime-series lst-idx)
  (map (lambda ((idx : Index)) (vector-ref (DatetimeSeries-data datetime-series) idx))
       lst-idx))

; This function consumes a datetime series and returns its
; data vector.
(: datetime-series-data (DatetimeSeries -> (Vectorof RFDatetime)))
(define (datetime-series-data series)
  (DatetimeSeries-data series))

; This function consumes a datetime series and returns its
; index.
(: datetime-series-index (DatetimeSeries -> (U False RFIndex)))
(define (datetime-series-index series)
  (DatetimeSeries-index series))

(: datetime-series-null-value (DatetimeSeries -> RFNULL))
(define (datetime-series-null-value datetime-series)
  (DatetimeSeries-null-value datetime-series))

(: datetime-series-datetime-null-value (DatetimeSeries -> Datetime))
(define (datetime-series-datetime-null-value datetime-series)
  (DatetimeSeries-datetime-null-value datetime-series))

; This function consumes an integer series and an index and
; returns a vector of values in the range [0:index] in the series.
(: datetime-series-range (DatetimeSeries Index Index -> (Vectorof RFDatetime)))
(define (datetime-series-range series start end)
   (vector-copy (datetime-series-data series) start end))

; This function consumes a series and a Label and returns
; the list of values at that Label in the series.
(: datetime-series-label-ref (DatetimeSeries Label -> (Listof RFDatetime)))
(define (datetime-series-label-ref series label)
  (datetime-series-iref series (key->lst-idx (assert (DatetimeSeries-index series)) label)))

; This function consumes a series and a Label and returns
; the list of values at that Label in the series.
(: datetime-series-index-ref (DatetimeSeries IndexDataType -> (Listof RFDatetime)))
(define (datetime-series-index-ref series item)
  (datetime-series-iref series (key->lst-idx (assert (DatetimeSeries-index series)) item)))

; This function consumes an integer series and returns the
; length of that series.
(: datetime-series-length (DatetimeSeries -> Index))
(define (datetime-series-length series)
  (vector-length (DatetimeSeries-data series)))

(: in-datetime-series (Datetime DatetimeSeries -> Boolean))
(define (in-datetime-series val series)
  (if (vector-memq val (datetime-series-data series)) #t #f))

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

(define-predicate ListofDatetime? (Listof Datetime))

(: datetime-series-loc-multi-index (DatetimeSeries (U (Listof String) ListofListofString) -> (U RFDatetime DatetimeSeries)))
(define (datetime-series-loc-multi-index datetime-series label)
  (unless (DatetimeSeries-index datetime-series)
    (let ((k (current-continuation-marks)))
      (raise (make-exn:fail:contract "datetime-series must have a label index." k))))

  (: get-index-val ((Listof String) -> Symbol))
  (define (get-index-val label)
    (string->symbol (string-append (string-join label "\t") "\t")))
  
  (if (ListofListofString? label)
      (datetime-series-loc datetime-series (map get-index-val label))
      (datetime-series-loc datetime-series (get-index-val label))))

(: datetime-series-loc (DatetimeSeries (U Label (Listof Label) (Listof Boolean)) -> (U RFDatetime DatetimeSeries)))
(define (datetime-series-loc datetime-series label)
  (unless (DatetimeSeries-index datetime-series)
    (let ((k (current-continuation-marks)))
      (raise (make-exn:fail:contract "datetime-series must have a label index." k))))

  (if (ListofBoolean? label)
      (datetime-series-loc-boolean datetime-series label)
      (let ((associated-indices-length : (Listof Integer)
                                       (map (lambda ([l : Label]) (length (datetime-series-label-ref datetime-series l))) (convert-to-label-lst label)))
            (vals : (Vectorof Datetime)
             (if (list? label)
                 (list->vector (assert (flatten (map (lambda ([l : Label]) (datetime-series-label-ref datetime-series l)) label)) ListofDatetime?))
                 (list->vector (assert (datetime-series-label-ref datetime-series label) ListofDatetime?)))))

        (if (= (vector-length vals) 1)
            (vector-ref vals 0)
            (new-DatetimeSeries vals #:index (build-index-from-list (build-labels-by-count (convert-to-label-lst label) associated-indices-length)) #:fill-null (datetime-series-null-value datetime-series))))))

; index based
(: datetime-series-iloc (DatetimeSeries (U Index (Listof Index)) -> (U RFDatetime DatetimeSeries)))
(define (datetime-series-iloc datetime-series idx)
  (let ((referencer (datetime-series-referencer datetime-series)))
  (if (list? idx)
      ; get labels from SIndex that refer to given indicies
      ; make a new index from these labels using build-index-from-labels
      ; sub-vector the data vector to get the data and create a new-ISeries
      (new-DatetimeSeries
       (for/vector: : (Vectorof RFDatetime) ([i idx])
         (vector-ref (datetime-series-data datetime-series) i))
       #:index (if (not (DatetimeSeries-index datetime-series))
                   #f
                   (build-index-from-list (map (lambda ([i : Index]) (idx->key (assert (DatetimeSeries-index datetime-series)) i)) idx))))
      (referencer idx))))

(: datetime-series-iloc-range (DatetimeSeries Index Index -> DatetimeSeries))
(define (datetime-series-iloc-range datetime-series start end)
  ; use vector-copy library method
  (new-DatetimeSeries
   (vector-copy (datetime-series-data datetime-series) start end)
   #:index (if (not (DatetimeSeries-index datetime-series))
               #f
               (build-index-from-list (map (lambda ([i : Index]) (idx->key (assert (DatetimeSeries-index datetime-series)) i)) (range start end))))))


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

(: datetime-series-loc-boolean (DatetimeSeries (Listof Boolean) -> (U RFDatetime DatetimeSeries)))
(define (datetime-series-loc-boolean datetime-series boolean-lst)
  (: data (Vectorof RFDatetime))
  (define data (datetime-series-data datetime-series))

  (: new-data (Vectorof RFDatetime))
  (define new-data (make-vector (length (filter true? boolean-lst)) DEFAULT_NULL_VALUE))
  
  (define data-idx 0)
  (define new-data-idx 0)

  (if (= (length boolean-lst) 1)
      (if (list-ref boolean-lst 0)
          (vector-ref data 0)
          ; empty iseries
          (new-DatetimeSeries (vector)))

      ; to achieve the single result case
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
      (new-DatetimeSeries new-data)))

; ***********************************************************

; ***********************************************************

(: map/datetime-series-data (DatetimeSeries (Datetime -> Datetime) -> DatetimeSeries))
(define (map/datetime-series-data series fn)
  (let ((old-data (datetime-series-data series)))
    (new-DatetimeSeries
     (build-vector (vector-length old-data)
                   (λ: ((idx : Natural))
                     (fn (assert (vector-ref old-data idx) Datetime?)))))))

(: bop/datetime-series (DatetimeSeries DatetimeSeries (Datetime Datetime -> Datetime) -> DatetimeSeries))
(define (bop/datetime-series datetime-series-1 datetime-series-2 bop)
  (define v1 (DatetimeSeries-data datetime-series-1))
  (define v2 (DatetimeSeries-data datetime-series-2))
  (define: len : Index (vector-length v1))
  
  (unless (eqv? len (vector-length v2))
	  (error 'bop/datetime-series "Series must be of equal length."))

  (define: v-bop : (Vectorof Datetime) (make-vector len #{DEFAULT_NULL_VALUE : Datetime}))

  ; Do loop returns DatetimeSeries, idx to 0 and increments by 1 Fixnum on
  ; each iteration (this is the step-exprs). When the loop has gone
  ; through the whole vector, the resulting new ISeries is returned
  ; which the v-bop as the data.
  (do: : DatetimeSeries ([idx : Fixnum 0 (unsafe-fx+ idx #{1 : Fixnum})])
       ((= idx len) (new-DatetimeSeries v-bop))
       (vector-set! v-bop idx (bop (assert (vector-ref v1 idx) Datetime?)
				   (assert (vector-ref v2 idx) Datetime?)))))

(: comp/datetime-series (DatetimeSeries DatetimeSeries (Datetime Datetime -> Boolean) -> BSeries))
(define (comp/datetime-series datetime-series-1 datetime-series-2 comp)
  (define v1 (DatetimeSeries-data datetime-series-1))
  (define v2 (DatetimeSeries-data datetime-series-2))
  (define: len : Index (vector-length v1))
  
  (unless (eqv? len (vector-length v2))
	  (error 'comp/datetime-series "Series must be of equal length."))

  (define: v-comp : (Vectorof Boolean) (make-vector len #f))

  ; Do loop returns DatetimeSeries, idx to 0 and increments by 1 Fixnum on
  ; each iteration (this is the step-exprs). When the loop has gone
  ; through the whole vector, the resulting new ISeries is returned
  ; which the v-bop as the data.
  (do: : BSeries ([idx : Fixnum 0 (unsafe-fx+ idx #{1 : Fixnum})])
       ((= idx len) (new-BSeries v-comp))
       (vector-set! v-comp idx (comp (assert (vector-ref v1 idx) Datetime?)
				   (assert (vector-ref v2 idx) Datetime?)))))
; ***********************************************************

; ***********************************************************
; These functions apply addition, subtraction,
; using datetime+ and datetime- and the bop/datetime-series function defined
; above.

(: date-to-seconds (Datetime -> Integer))
(define (date-to-seconds datetime)
  (let* ([rf-date : Date (datetime-date datetime)]
         [time : Time (datetime-time datetime)]
         [year : Integer (rf-date-year rf-date)]         
         [month : Integer (rf-date-month rf-date)]         
         [day : Integer (rf-date-day rf-date)]         
         [offset : Integer (time-offset time)]         
         [hour : Integer (time-hour time)]         
         [minute : Integer (time-minute time)]         
         [second : Integer (time-second time)]         
         [milli : Integer (time-milli time)]        

         [racket-date : date (date (assert second exact-nonnegative-integer?) (assert minute exact-nonnegative-integer?) (assert hour exact-nonnegative-integer?)
                                     (assert day exact-nonnegative-integer?) (assert month exact-nonnegative-integer?) (assert year exact-nonnegative-integer?) 0 0 #f offset)])

    (date->seconds racket-date)))

(: datetime+ (Datetime Datetime -> Datetime))
(define (datetime+ datetime-1 datetime-2)
  (let* ([date-seconds-1 : Integer (date-to-seconds datetime-1)]
         [date-seconds-2 : Integer (date-to-seconds datetime-2)]
         [result : date (seconds->date (+ date-seconds-1 date-seconds-2))])

    (Datetime (Date (date-year result) (date-month result) (date-day result))
              (Time (date-time-zone-offset result) (date-hour result) (date-minute result) (date-second result) (+ (time-milli (datetime-time datetime-1)) (time-milli (datetime-time datetime-2)))))))

(: datetime- (Datetime Datetime -> Datetime))
(define (datetime- datetime-1 datetime-2)
  (let* ([date-seconds-1 : Integer (date-to-seconds datetime-1)]
         [date-seconds-2 : Integer (date-to-seconds datetime-2)]
         [result : date (seconds->date (- date-seconds-1 date-seconds-2))])

    (Datetime (Date (date-year result) (date-month result) (date-day result))
              (Time (date-time-zone-offset result) (date-hour result) (date-minute result) (date-second result) (- (time-milli (datetime-time datetime-1)) (time-milli (datetime-time datetime-2)))))))

(: datetime< (Datetime Datetime -> Boolean))
(define (datetime< datetime-1 datetime-2)
  (let* ([date-seconds-1 : Integer (date-to-seconds datetime-1)]
         [date-seconds-2 : Integer (date-to-seconds datetime-2)])

    (< date-seconds-1 date-seconds-2)))

(: datetime<= (Datetime Datetime -> Boolean))
(define (datetime<= datetime-1 datetime-2)
  (let* ([date-seconds-1 : Integer (date-to-seconds datetime-1)]
         [date-seconds-2 : Integer (date-to-seconds datetime-2)])

    (<= date-seconds-1 date-seconds-2)))

(: datetime> (Datetime Datetime -> Boolean))
(define (datetime> datetime-1 datetime-2)
  (let* ([date-seconds-1 : Integer (date-to-seconds datetime-1)]
         [date-seconds-2 : Integer (date-to-seconds datetime-2)])

    (> date-seconds-1 date-seconds-2)))

(: datetime>= (Datetime Datetime -> Boolean))
(define (datetime>= datetime-1 datetime-2)
  (let* ([date-seconds-1 : Integer (date-to-seconds datetime-1)]
         [date-seconds-2 : Integer (date-to-seconds datetime-2)])

    (>= date-seconds-1 date-seconds-2)))

(: datetime= (Datetime Datetime -> Boolean))
(define (datetime= datetime-1 datetime-2)
  (let* ([date-seconds-1 : Integer (date-to-seconds datetime-1)]
         [date-seconds-2 : Integer (date-to-seconds datetime-2)])

    (= date-seconds-1 date-seconds-2)))

(: datetime!= (Datetime Datetime -> Boolean))
(define (datetime!= datetime-1 datetime-2)
  (let* ([date-seconds-1 : Integer (date-to-seconds datetime-1)]
         [date-seconds-2 : Integer (date-to-seconds datetime-2)])

    (not (= date-seconds-1 date-seconds-2))))

(: +/datetime-series (DatetimeSeries DatetimeSeries -> DatetimeSeries))
(define (+/datetime-series datetime-1 datetime-2)
  (bop/datetime-series datetime-1 datetime-2 datetime+))

(: -/datetime-series (DatetimeSeries DatetimeSeries -> DatetimeSeries))
(define (-/datetime-series datetime-1 datetime-2)
  (bop/datetime-series datetime-1 datetime-2 datetime-))

(: >/datetime-series (DatetimeSeries DatetimeSeries -> BSeries))
(define (>/datetime-series datetime-1 datetime-2)
  (comp/datetime-series datetime-1 datetime-2 datetime>))

(: </datetime-series (DatetimeSeries DatetimeSeries -> BSeries))
(define (</datetime-series datetime-1 datetime-2)
  (comp/datetime-series datetime-1 datetime-2 datetime<))

(: >=/datetime-series (DatetimeSeries DatetimeSeries -> BSeries))
(define (>=/datetime-series datetime-1 datetime-2)
  (comp/datetime-series datetime-1 datetime-2 datetime>=))

(: <=/datetime-series (DatetimeSeries DatetimeSeries -> BSeries))
(define (<=/datetime-series datetime-1 datetime-2)
  (comp/datetime-series datetime-1 datetime-2 datetime<=))

(: =/datetime-series (DatetimeSeries DatetimeSeries -> BSeries))
(define (=/datetime-series datetime-1 datetime-2)
  (comp/datetime-series datetime-1 datetime-2 datetime=))

(: !=/datetime-series (DatetimeSeries DatetimeSeries -> BSeries))
(define (!=/datetime-series datetime-1 datetime-2)
  (comp/datetime-series datetime-1 datetime-2 datetime!=))

(: freq-offset (Symbol -> Real))
(define (freq-offset freq)
  (let ([milli : Real (/ 1 1000)]
        [second : Real 1]
        [minute : Real 60]
        [hour : Real 3600]
        [day : Real 86400]
        [week : Real 604800]
        [month : Real 2.628e+6]
        [year : Real 3.154e+7])
  (cond
   [(equal? freq 'MS) milli]
   [(equal? freq 'S) second]
   [(equal? freq 'M) minute]
   [(equal? freq 'H) hour]   
   [(equal? freq 'D) day]
   [(equal? freq 'W) week]
   [(equal? freq 'MO) month]
   [(equal? freq 'Y) year]
   [else (error "invalid freq")])))

(: datetime-range (Datetime (Option Symbol) (Option Index) (Option Datetime) -> (Listof Datetime)))
(define (datetime-range datetime freq periods end)
  (let* ([date-seconds : Integer (date-to-seconds datetime)]
        [offset : Real (freq-offset (if (not freq) 'D freq))]
        [interval : Real (if (not end) offset (* offset (assert (if (not periods) 1 periods))))]
        [end-date-seconds : Real (if (not end)
                              (+ date-seconds (* offset (assert (if (not periods) 1 periods))))
                              (date-to-seconds end))]
        [end-loop : Real (if (not end) 
                             (/  (- end-date-seconds date-seconds) (assert offset))
                             (/ (- end-date-seconds date-seconds) (assert interval)))])


    (if (<= date-seconds end-date-seconds)
        (for/list: : (Listof Datetime) ([i (range (exact-ceiling end-loop))])

          (let* ((result : date (seconds->date (exact-ceiling (+ date-seconds (* i interval)))))
                (dt (Datetime (Date (date-year result) (date-month result) (date-day result))
                                    (Time (date-time-zone-offset result) (date-hour result) (date-minute result) (date-second result) (time-milli (datetime-time datetime))))))
           
           dt))
        (list datetime))
    ))

;(range (/ (- (date-to-seconds (Datetime (Date 2018 6 19) (Time 0 0 0 0 0))) (date-to-seconds (Datetime (Date 2018 5 19) (Time 0 0 0 0 0)))) 864 00))

;(datetime-range (Datetime (Date 2018 5 19) (Time 0 0 0 0 0)) 'MO 2 (Datetime (Date 2018 10 23) (Time 0 0 0 0 0)))

;(datetime-range (Datetime (Date 1975 1 1) (Time 0 0 0 0 0)) 'MO 100 #f)

; ***********************************************************
;;DatetimeSeries groupby

(define-type Key String)
(define-type GroupHash (HashTable Key (Listof RFDatetime)))

; This function is self-explanatory, it consumes no arguments
; and creates a hash map which will represent a JoinHash.
(: make-group-hash (-> GroupHash))
(define (make-group-hash)
  (make-hash))

; Used to determine the groups for the groupby. If by is a function, it’s called on each value of the object’s index.
; The Series VALUES will be used to determine the groups if by-value is set to true.
(: datetime-series-groupby (DatetimeSeries [#:by-value Boolean] -> GroupHash))
(define (datetime-series-groupby datetime-series #:by-value [by-value #f])
  (define: group-index : GroupHash (make-group-hash))  

  (let ((len (datetime-series-length datetime-series))
        (k (current-continuation-marks)))
    (if (zero? len)
	(raise (make-exn:fail:contract "datetime-series can't be empty on groupby." k))
	(begin          
	  (do ((i 0 (add1 i)))
	      ((>= i len) group-index)
	    (let* ((datetime-val : (U RFDatetime DatetimeSeries) (datetime-series-iloc datetime-series (assert i index?)))
                   (datetime-list : (Listof RFDatetime) (if (RFDatetime? datetime-val) (list datetime-val) (vector->list (DatetimeSeries-data datetime-val))))
                   (key (if by-value
                            (datetime->string (assert (datetime-series-iloc datetime-series (assert i index?)) Datetime?) #f)
                            (if (DatetimeSeries-index datetime-series)
                                (idx->key (assert (DatetimeSeries-index datetime-series)) (assert i index?))
                                (assert i index?))))
                  (key-str : String (cond
                                      [(symbol? key) (symbol->string key)]
                                      [(number? key) (number->string key)]
                                      ; pretty-format anything else
                                      [else (pretty-format key)])))              
              (hash-update! group-index key-str
			      (λ: ((val : (Listof RFDatetime)))                                
				  (append datetime-list val))
			      (λ () (list)))))))))

; ***********************************************************

; ***********************************************************
; DatetimeSeries Filtering
; ***********************************************************
(: datetime-series-index-from-predicate (DatetimeSeries (RFDatetime -> Boolean) -> RFIndex))
(define (datetime-series-index-from-predicate datetime-series pred)  
  (build-index-from-list
   (for/list : (Listof IndexDataType)
     ([val (datetime-series-data datetime-series)]
      [n (in-naturals)]
      #:when (pred (assert val date?)))
     (if (datetime-series-index datetime-series)
         (idx->key (assert (datetime-series-index datetime-series)) (assert n index?))
         (assert n index?)))))

(: datetime-series-data-idxes-from-predicate (DatetimeSeries (RFDatetime -> Boolean) -> (Listof Index)))
(define (datetime-series-data-idxes-from-predicate datetime-series pred)    
   (for/list : (Listof Index)
     ([val (datetime-series-data datetime-series)]
      [n (in-naturals)]
      #:when (pred (assert val date?)))
         (assert n index?)))

(: datetime-series-data-idxes-from-predicate-not (DatetimeSeries (RFDatetime -> Boolean) -> (Listof Index)))
(define (datetime-series-data-idxes-from-predicate-not datetime-series pred)    
   (for/list : (Listof Index)
     ([val (datetime-series-data datetime-series)]
      [n (in-naturals)]
      #:when (pred (assert val date?)))
         (assert n index?)))

(: datetime-series-filter (DatetimeSeries (RFDatetime -> Boolean) -> DatetimeSeries))
(define (datetime-series-filter datetime-series filter-function)
  ; need to use new filtered data to get the new index
  ; setting #f is naive  
  (new-DatetimeSeries (vector-filter filter-function (datetime-series-data datetime-series)) #:index (datetime-series-index-from-predicate datetime-series filter-function)))

(: datetime-series-index-from-predicate-not (DatetimeSeries (RFDatetime -> Boolean) -> RFIndex))
(define (datetime-series-index-from-predicate-not datetime-series pred)  
  (build-index-from-list
   (for/list : (Listof IndexDataType)
     ([val (datetime-series-data datetime-series)]
      [n (in-naturals)]
      #:when (not (pred (assert val date?))))
     (if (datetime-series-index datetime-series)
         (idx->key (assert (datetime-series-index datetime-series)) (assert n index?))
         (assert n index?)))))

(: datetime-series-filter-not (DatetimeSeries (RFDatetime -> Boolean) -> DatetimeSeries))
(define (datetime-series-filter-not datetime-series filter-function)
  (new-DatetimeSeries (vector-filter-not filter-function (datetime-series-data datetime-series)) #:index (datetime-series-index-from-predicate-not datetime-series filter-function)))
; ***********************************************************
; DatetimeSeries Filtering
; ***********************************************************

; ***********************************************************
; DatetimeSeries Sorting
; ***********************************************************
(: datetime-series-data->pair-list (DatetimeSeries -> (Listof (Pair Datetime Index))))
(define (datetime-series-data->pair-list datetime-series)
  (for/list : (Listof (Pair Datetime Index))
    ([val (datetime-series-data datetime-series)]
     [n (in-naturals)])
    (cons (assert val Datetime?) (assert n index?))))

(: datetime-series-sort-pair-list ((Listof (Pairof Datetime Index)) -> (Listof (Pairof Datetime Index))))
(define (datetime-series-sort-pair-list pair-lst)
  ((inst sort (Pair Datetime Index) Datetime) pair-lst datetime< #:key (λ ((p : (Pair Datetime Index))) (car p)) #:cache-keys? #t))

(: datetime-series-sort-pair-list-descending ((Listof (Pairof Datetime Index)) -> (Listof (Pairof Datetime Index))))
(define (datetime-series-sort-pair-list-descending pair-lst)
  ((inst sort (Pair Datetime Index) Datetime) pair-lst datetime> #:key (λ ((p : (Pair Datetime Index))) (car p)) #:cache-keys? #t))

(: datetime-series-get-sorted-data ((Listof (Pairof Datetime Index)) -> (Listof Datetime)))
(define (datetime-series-get-sorted-data pair-lst)
  (map (lambda ((pairing : (Pairof Datetime Index))) (car pairing)) pair-lst))

(: datetime-series-get-original-idxes ((Listof (Pairof Datetime Index)) -> (Listof Index)))
(define (datetime-series-get-original-idxes pair-lst)
  (map (lambda ((pairing : (Pairof Datetime Index))) (cdr pairing)) pair-lst))

(: datetime-series-index-from-idxes (DatetimeSeries (Listof Index) -> RFIndex))
(define (datetime-series-index-from-idxes datetime-series idx-list)  
  (build-index-from-list
   (for/list : (Listof IndexDataType)
     ([idx idx-list])
     (if (datetime-series-index datetime-series)
         (idx->key (assert (datetime-series-index datetime-series)) (assert idx index?))
         (assert idx index?)))))

(: datetime-series-sort (DatetimeSeries -> DatetimeSeries))
(define (datetime-series-sort datetime-series)
  (let* ([datetime-series-sorted-pairs (datetime-series-sort-pair-list (datetime-series-data->pair-list datetime-series))]
         [datetime-series-index (datetime-series-index-from-idxes datetime-series (datetime-series-get-original-idxes datetime-series-sorted-pairs))]
         [datetime-series-sorted-data (datetime-series-get-sorted-data datetime-series-sorted-pairs)])
    (new-DatetimeSeries datetime-series-sorted-data #:index datetime-series-index)))

(: datetime-series-sort-descending (DatetimeSeries -> DatetimeSeries))
(define (datetime-series-sort-descending datetime-series)
  (let* ([datetime-series-sorted-pairs (datetime-series-sort-pair-list-descending (datetime-series-data->pair-list datetime-series))]
         [datetime-series-index (datetime-series-index-from-idxes datetime-series (datetime-series-get-original-idxes datetime-series-sorted-pairs))]
         [datetime-series-sorted-data (datetime-series-get-sorted-data datetime-series-sorted-pairs)])
    (new-DatetimeSeries datetime-series-sorted-data #:index datetime-series-index)))
; ***********************************************************
; DatetimeSeries Sorting
; ***********************************************************