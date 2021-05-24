;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*    Author: Shubham Kahal
;*    File: date-series.rkt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket

(require typed/racket/date)

(require
 racket/unsafe/ops
 (only-in "indexed-series.rkt"
	  RFIndex RFIndex? RFNULL RFNoData
           build-index-from-list IndexDataType extract-index
          Label LabelIndex-index
          LabelIndex label-index label->lst-idx key->lst-idx
          idx->key is-indexed? ListofIndexDataType? ListofIndex?
          ListofListofString ListofListofString?
          ListofBoolean? ListofFixnum?)
 (only-in "boolean-series.rkt"
          new-BSeries BSeries))

;(date->seconds (date 1 2 3 4 5 1971 5 8 #f 0))

; ***********************************************************
; Provide functions in this file to other files.
(provide
 (struct-out DateSeries)
 DateSeries? RFDate RFDate? (rename-out [DEFAULT_NULL_VALUE DATE_SERIES_DEFAULT_NULL_VALUE] [GroupHash date-series-grouphash]))

(provide:
 [new-DateSeries ((U (Vectorof date) (Sequenceof date) (Sequenceof RFDate)) [#:index (Option (U (Listof IndexDataType) RFIndex))] [#:fill-null RFNULL]  -> DateSeries)]
 [set-DateSeries-index (DateSeries (U (Listof IndexDataType) RFIndex) -> DateSeries)]
 [date-series-iref (DateSeries (Listof Index) -> (Listof RFDate))]
 [date-series-index-ref (DateSeries IndexDataType -> (Listof RFDate))]
 [date-series-loc-multi-index (DateSeries (U (Listof String) ListofListofString) -> (U RFDate DateSeries))]
 [date-series-loc-boolean (DateSeries (Listof Boolean) -> (U RFDate DateSeries))]
 [date-series-loc (DateSeries (U Label (Listof Label) (Listof Boolean)) -> (U RFDate DateSeries))]
 [date-series-iloc (DateSeries (U Index (Listof Index)) -> (U RFDate DateSeries))]
 [date-series-iloc-range (DateSeries Index Index -> DateSeries)]
 [date-series-label-ref (DateSeries Label -> (Listof RFDate))]
 [date-series-range (DateSeries Index Index -> (Vectorof RFDate))]
 [date-series-length (DateSeries -> Index)]
 [date-series-referencer (DateSeries -> (Index -> RFDate))]
 [date-series-data (DateSeries -> (Vectorof RFDate))]
 [date-series-index (DateSeries -> (U False RFIndex))]
 [in-date-series (date DateSeries -> Boolean)]
 [date-series-null-value (DateSeries -> RFNULL)]
 [date-series-date-null-value (DateSeries -> date)]
 [map/date-series-data (DateSeries (date -> date) -> DateSeries)]
 [date-range (date (Option Symbol) (Option Index) (Option date) -> (Listof RFDate))]

 [bop/date-series (DateSeries DateSeries (date date -> date) -> DateSeries)]
 [comp/date-series (DateSeries DateSeries (date date -> Boolean) -> BSeries)]
 [+/date-series (DateSeries DateSeries -> DateSeries)]
 [-/date-series (DateSeries DateSeries -> DateSeries)]
 ;[+./date-series (DateSeries date -> DateSeries)]
 ;[-./date-series (DateSeries date -> DateSeries)]
 [>/date-series (DateSeries DateSeries -> BSeries)]
 [</date-series (DateSeries DateSeries -> BSeries)]
 [>=/date-series (DateSeries DateSeries -> BSeries)]
 [<=/date-series (DateSeries DateSeries -> BSeries)]
 [=/date-series (DateSeries DateSeries -> BSeries)]
 [!=/date-series (DateSeries DateSeries -> BSeries)]
 
 [date-series-print (DateSeries [#:output-port Output-Port] -> Void)]

 [date-series-groupby (DateSeries [#:by-value Boolean] -> GroupHash)]
 ; in Pandas, fillna
 [set-DateSeries-null-value (DateSeries RFNULL -> DateSeries)]
 [set-DateSeries-date-null-value-inplace (DateSeries date -> Void)]

 [derive-date-value (DateSeries RFDate -> date)])
; ***********************************************************

#|
(struct	 date	 
(second
 	minute
 	hour
 	day
 	month
 	year
 	week-day
 	year-day
 	dst?
 	time-zone-offset)
  #:extra-constructor-name make-date
  #:transparent)
  second : (integer-in 0 60)
  minute : (integer-in 0 59)
  hour : (integer-in 0 23)
  day : (integer-in 1 31)
  month : (integer-in 1 12)
  year : exact-integer?
  week-day : (integer-in 0 6)
  year-day : (integer-in 0 365)
  dst? : boolean?
  time-zone-offset : exact-integer?
|#

(define-type RFDate (U date RFNoData))
(define-predicate RFDate? RFDate)
(define DEFAULT_NULL_VALUE : date (current-date))
(struct DateSeries
  ([index : (Option RFIndex)]
   [data : (Vectorof RFDate)]
   ; when the null-value is not a fixnum?, the fixnum-null-value is set to 0
   [null-value : RFNULL]
   ; needed for type checking purposes and to do proper arithmetic operations in numeric series
   [date-null-value : date])
  #:mutable
  #:transparent)

(: make-RFDate-vector ((U (Sequenceof date) (Sequenceof RFDate)) -> (Vectorof RFDate)))
(define (make-RFDate-vector seq)
  (let ((vec : (Vectorof RFDate) (list->vector (sequence->list seq))))
    vec))

; Consumes a Vector of Fixnum and a list of Labels which
; can come in list form or SIndex form and produces a DateSeries
; struct object.
(: new-DateSeries ((U (Vectorof date) (Sequenceof date) (Sequenceof RFDate)) [#:index (Option (U (Listof IndexDataType) RFIndex))] [#:fill-null RFNULL] -> DateSeries))
(define (new-DateSeries data #:index [labels #f] #:fill-null [null-value DEFAULT_NULL_VALUE]) 

  (define data-vector : (Vectorof RFDate) (make-RFDate-vector data))

  (: check-mismatch (RFIndex -> Void))
  (define (check-mismatch index)    
    (let ((index-length (apply + (for/list: : (Listof Index)
                                   ([value (in-hash-values (extract-index index))])
                                   (length (assert value ListofIndex?))))))

      (unless (eq? (vector-length data-vector) index-length)
        (let ((k (current-continuation-marks)))
          (raise (make-exn:fail:contract "Cardinality of a Series' data and labels must be equal" k))))
      (void)))

  (define date-null-value : date (if (date? null-value) null-value DEFAULT_NULL_VALUE))

  (if (RFIndex? labels)
      (begin
	(check-mismatch (assert labels))
	(DateSeries labels data-vector null-value date-null-value))
      (if labels
	  (let ((index (build-index-from-list (assert labels ListofIndexDataType?))))
	    (check-mismatch index)
	    (DateSeries index data-vector null-value date-null-value))
	  (DateSeries #f data-vector null-value date-null-value))))
; ***********************************************************

; ***********************************************************
(: set-DateSeries-index (DateSeries (U (Listof IndexDataType) RFIndex) -> DateSeries))
(define (set-DateSeries-index date-series labels)
  (new-DateSeries (date-series-data date-series) #:index labels))

(: set-DateSeries-null-value (DateSeries RFNULL -> DateSeries))
(define (set-DateSeries-null-value date-series null-value)
  (new-DateSeries (date-series-data date-series) #:index (date-series-index date-series) #:fill-null null-value))

(: set-DateSeries-date-null-value-inplace (DateSeries date -> Void))
(define (set-DateSeries-date-null-value-inplace date-series null-value)
  (set-DateSeries-date-null-value! date-series null-value))
; ***********************************************************

; ***********************************************************
(: date-series-print (DateSeries [#:output-port Output-Port] -> Void))
(define (date-series-print date-series #:output-port [port (current-output-port)])
  (define date-v (date-series-data date-series))
  (define v (date-series-data date-series))
  (let ((len (vector-length v))
	(out (current-output-port)))
    (if (zero? len)
	(displayln "Empty $DateSeries" port)
	(begin
          (displayln "*********" port)
          (displayln "$DateSeries" port)
          (displayln "*********" port)
	  (do ((i 0 (add1 i)))
	      ((>= i len) (void))
	    (let ((date (vector-ref date-v i))
                  (num (vector-ref v i)))
              (if (date-series-index date-series)                  
                  (display (idx->key (assert (date-series-index date-series)) (assert i index?)) port)
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
(: date-series-referencer (DateSeries -> (Index -> RFDate)))
(define (date-series-referencer date-series)
  (let ((data (date-series-data date-series)))
    (λ: ((idx : Index))
	(vector-ref data idx))))

; This function consumes an integer series and an index and
; returns the value at that index in the series.
(: date-series-iref (DateSeries (Listof Index) -> (Listof RFDate)))
(define (date-series-iref date-series lst-idx)
  (map (lambda ((idx : Index)) (vector-ref (date-series-data date-series) idx))
       lst-idx))

; This function consumes a date series and returns its
; data vector.
(: date-series-data (DateSeries -> (Vectorof RFDate)))
(define (date-series-data series)
  (DateSeries-data series))

; This function consumes a date series and returns its
; index.
(: date-series-index (DateSeries -> (U False RFIndex)))
(define (date-series-index series)
  (DateSeries-index series))

(: date-series-null-value (DateSeries -> RFNULL))
(define (date-series-null-value date-series)
  (DateSeries-null-value date-series))

(: date-series-date-null-value (DateSeries -> date))
(define (date-series-date-null-value date-series)
  (DateSeries-date-null-value date-series))

; This function consumes an integer series and an index and
; returns a vector of values in the range [0:index] in the series.
(: date-series-range (DateSeries Index Index -> (Vectorof RFDate)))
(define (date-series-range series start end)
   (vector-copy (date-series-data series) start end))

; This function consumes a series and a Label and returns
; the list of values at that Label in the series.
(: date-series-label-ref (DateSeries Label -> (Listof RFDate)))
(define (date-series-label-ref series label)
  (date-series-iref series (key->lst-idx (assert (DateSeries-index series)) label)))

; This function consumes a series and a Label and returns
; the list of values at that Label in the series.
(: date-series-index-ref (DateSeries IndexDataType -> (Listof RFDate)))
(define (date-series-index-ref series item)
  (date-series-iref series (key->lst-idx (assert (DateSeries-index series)) item)))

; This function consumes an integer series and returns the
; length of that series.
(: date-series-length (DateSeries -> Index))
(define (date-series-length series)
  (vector-length (date-series-data series)))

(: in-date-series (date DateSeries -> Boolean))
(define (in-date-series val series)
  (if (vector-memq val (date-series-data series)) #t #f))

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

(define-predicate Listofdate? (Listof date))

(: date-series-loc-multi-index (DateSeries (U (Listof String) ListofListofString) -> (U RFDate DateSeries)))
(define (date-series-loc-multi-index date-series label)
  (unless (DateSeries-index date-series)
    (let ((k (current-continuation-marks)))
      (raise (make-exn:fail:contract "date-series must have a label index." k))))

  (: get-index-val ((Listof String) -> Symbol))
  (define (get-index-val label)
    (string->symbol (string-append (string-join label "::") "::")))
  
  (if (ListofListofString? label)
      (date-series-loc date-series (map get-index-val label))
      (date-series-loc date-series (get-index-val label))))

(: date-series-loc (DateSeries (U Label (Listof Label) (Listof Boolean)) -> (U RFDate DateSeries)))
(define (date-series-loc date-series label)
  (unless (DateSeries-index date-series)
    (let ((k (current-continuation-marks)))
      (raise (make-exn:fail:contract "date-series must have a label index." k))))

  (if (ListofBoolean? label)
      (date-series-loc-boolean date-series label)
      (let ((associated-indices-length : (Listof Integer)
                                       (map (lambda ([l : Label]) (length (date-series-label-ref date-series l))) (convert-to-label-lst label)))
            (vals : (Vectorof RFDate)
             (if (list? label)
                 (list->vector (assert (flatten (map (lambda ([l : Label]) (date-series-label-ref date-series l)) label)) Listofdate?))
                 (list->vector (assert (date-series-label-ref date-series label) Listofdate?)))))

        (if (= (vector-length vals) 1)
            (vector-ref vals 0)
            (new-DateSeries vals #:index (build-index-from-list (build-labels-by-count (convert-to-label-lst label) associated-indices-length)))))))

; index based
(: date-series-iloc (DateSeries (U Index (Listof Index)) -> (U RFDate DateSeries)))
(define (date-series-iloc date-series idx)
  (let ((referencer (date-series-referencer date-series)))
  (if (list? idx)
      ; get labels from SIndex that refer to given indicies
      ; make a new index from these labels using build-index-from-labels
      ; sub-vector the data vector to get the data and create a new-ISeries
      (new-DateSeries
       (for/vector: : (Vectorof RFDate) ([i idx])
         (vector-ref (date-series-data date-series) i))
       #:index (if (not (date-series-index date-series))
                   #f
                   (build-index-from-list (map (lambda ([i : Index]) (idx->key (assert (date-series-index date-series)) i)) idx))))
      (referencer idx))))

(: date-series-iloc-range (DateSeries Index Index -> DateSeries))
(define (date-series-iloc-range date-series start end)
  ; use vector-copy library method
  (new-DateSeries
   (vector-copy (date-series-data date-series) start end)
   #:index (if (not (date-series-index date-series))
               #f
               (build-index-from-list (map (lambda ([i : Index]) (idx->key (assert (date-series-index date-series)) i)) (range start end))))))

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

(: date-series-loc-boolean (DateSeries (Listof Boolean) -> (U RFDate DateSeries)))
(define (date-series-loc-boolean date-series boolean-lst)
  (: data (Vectorof RFDate))
  (define data (date-series-data date-series))

  (: new-data (Vectorof RFDate))
  (define new-data (make-vector (length (filter true? boolean-lst)) (current-date)))
  
  (define data-idx 0)
  (define new-data-idx 0)

  (if (= (length boolean-lst) 1)
      (if (list-ref boolean-lst 0)
          (vector-ref data 0)
          ; empty iseries
          (new-DateSeries (vector)))

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
      (new-DateSeries new-data)))

; ***********************************************************

; ***********************************************************

(: derive-date-value (DateSeries RFDate -> date))
(define (derive-date-value date-series val)
  (if (date? val) (assert val date?) (date-series-date-null-value date-series)))

(: map/date-series-data (DateSeries (date -> date) -> DateSeries))
(define (map/date-series-data series fn)
  (let ((old-data (date-series-data series)))
    (new-DateSeries (build-vector (vector-length old-data)
                                  (λ: ((idx : Natural))
                                    (fn (derive-date-value series (vector-ref old-data idx))))))))

(: bop/date-series (DateSeries DateSeries (date date -> date) -> DateSeries))
(define (bop/date-series date-series-1 date-series-2 bop)
  (define v1 (date-series-data date-series-1))
  (define v2 (date-series-data date-series-2))
  (define: len : Index (vector-length v1))
  
  (unless (eqv? len (vector-length v2))
	  (error 'bop/date-series "Series must be of equal length."))

  (define: v-bop : (Vectorof date) (make-vector len #{(current-date) : date}))

  ; Do loop returns DateSeries, idx to 0 and increments by 1 Fixnum on
  ; each iteration (this is the step-exprs). When the loop has gone
  ; through the whole vector, the resulting new ISeries is returned
  ; which the v-bop as the data.
  (do: : DateSeries ([idx : Fixnum 0 (unsafe-fx+ idx #{1 : Fixnum})])
       ((= idx len) (new-DateSeries v-bop))
       (vector-set! v-bop idx (bop (derive-date-value date-series-1 (vector-ref v1 idx))
				   (derive-date-value date-series-2 (vector-ref v2 idx))))))

(: comp/date-series (DateSeries DateSeries (date date -> Boolean) -> BSeries))
(define (comp/date-series date-series-1 date-series-2 comp)
  (define v1 (date-series-data date-series-1))
  (define v2 (date-series-data date-series-2))
  (define: len : Index (vector-length v1))
  
  (unless (eqv? len (vector-length v2))
	  (error 'comp/date-series "Series must be of equal length."))

  (define: v-comp : (Vectorof Boolean) (make-vector len #f))

  ; Do loop returns DateSeries, idx to 0 and increments by 1 Fixnum on
  ; each iteration (this is the step-exprs). When the loop has gone
  ; through the whole vector, the resulting new ISeries is returned
  ; which the v-bop as the data.
  (do: : BSeries ([idx : Fixnum 0 (unsafe-fx+ idx #{1 : Fixnum})])
       ((= idx len) (new-BSeries v-comp))
       (vector-set! v-comp idx (comp (derive-date-value date-series-1 (vector-ref v1 idx))
                                     (derive-date-value date-series-2 (vector-ref v2 idx))))))
; ***********************************************************

; ***********************************************************
; These functions apply addition, subtraction,
; using date+ and date- and the bop/date-series function defined
; above.

(: date+ (date date -> date))
(define (date+ date-1 date-2)
  (let* ([date-seconds-1 : Integer (date->seconds date-1)]
         [date-seconds-2 : Integer (date->seconds date-2)]
         [result : date (seconds->date (+ date-seconds-1 date-seconds-2))])

    result))

(: date- (date date -> date))
(define (date- date-1 date-2)
  (let* ([date-seconds-1 : Integer (date->seconds date-1)]
         [date-seconds-2 : Integer (date->seconds date-2)]
         [result : date (seconds->date (- date-seconds-1 date-seconds-2))])

    result))

(: date< (date date -> Boolean))
(define (date< date-1 date-2)
  (let* ([date-seconds-1 : Integer (date->seconds date-1)]
         [date-seconds-2 : Integer (date->seconds date-2)])

    (< date-seconds-1 date-seconds-2)))

(: date<= (date date -> Boolean))
(define (date<= date-1 date-2)
  (let* ([date-seconds-1 : Integer (date->seconds date-1)]
         [date-seconds-2 : Integer (date->seconds date-2)])

    (<= date-seconds-1 date-seconds-2)))

(: date> (date date -> Boolean))
(define (date> date-1 date-2)
  (let* ([date-seconds-1 : Integer (date->seconds date-1)]
         [date-seconds-2 : Integer (date->seconds date-2)])

    (> date-seconds-1 date-seconds-2)))

(: date>= (date date -> Boolean))
(define (date>= date-1 date-2)
  (let* ([date-seconds-1 : Integer (date->seconds date-1)]
         [date-seconds-2 : Integer (date->seconds date-2)])

    (>= date-seconds-1 date-seconds-2)))

(: date= (date date -> Boolean))
(define (date= date-1 date-2)
  (let* ([date-seconds-1 : Integer (date->seconds date-1)]
         [date-seconds-2 : Integer (date->seconds date-2)])

    (= date-seconds-1 date-seconds-2)))

(: date!= (date date -> Boolean))
(define (date!= date-1 date-2)
  (let* ([date-seconds-1 : Integer (date->seconds date-1)]
         [date-seconds-2 : Integer (date->seconds date-2)])

    (not (= date-seconds-1 date-seconds-2))))

(: +/date-series (DateSeries DateSeries -> DateSeries))
(define (+/date-series date-1 date-2)
  (bop/date-series date-1 date-2 date+))

(: -/date-series (DateSeries DateSeries -> DateSeries))
(define (-/date-series date-1 date-2)
  (bop/date-series date-1 date-2 date-))

(: >/date-series (DateSeries DateSeries -> BSeries))
(define (>/date-series date-1 date-2)
  (comp/date-series date-1 date-2 date>))

(: </date-series (DateSeries DateSeries -> BSeries))
(define (</date-series date-1 date-2)
  (comp/date-series date-1 date-2 date<))

(: >=/date-series (DateSeries DateSeries -> BSeries))
(define (>=/date-series date-1 date-2)
  (comp/date-series date-1 date-2 date>=))

(: <=/date-series (DateSeries DateSeries -> BSeries))
(define (<=/date-series date-1 date-2)
  (comp/date-series date-1 date-2 date<=))

(: =/date-series (DateSeries DateSeries -> BSeries))
(define (=/date-series date-1 date-2)
  (comp/date-series date-1 date-2 date=))

(: !=/date-series (DateSeries DateSeries -> BSeries))
(define (!=/date-series date-1 date-2)
  (comp/date-series date-1 date-2 date!=))

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

(: date-range (date (Option Symbol) (Option Index) (Option date) -> (Listof date)))
(define (date-range racket-date freq periods end)
  (let* ([date-seconds : Integer (date->seconds racket-date)]
        [offset : Real (freq-offset (if (not freq) 'D freq))]
        [interval : Real (if (not end) offset (* offset (assert (if (not periods) 1 periods))))]
        [end-date-seconds : Real (if (not end)
                              (+ date-seconds (* offset (assert (if (not periods) 1 periods))))
                              (date->seconds end))]
        [end-loop : Real (if (not end) 
                             (/  (- end-date-seconds date-seconds) (assert offset))
                             (/ (- end-date-seconds date-seconds) (assert interval)))])


    (if (<= date-seconds end-date-seconds)
        (for/list: : (Listof date) ([i (range (exact-ceiling end-loop))])

          (let* ((result : date (seconds->date (exact-ceiling (+ date-seconds (* i interval))))))
            result
            ))
        (list racket-date))
    ))

;(range (/ (- (date->seconds (date (Date 2018 6 19) (Time 0 0 0 0 0))) (date->seconds (date (Date 2018 5 19) (Time 0 0 0 0 0)))) 864 00))

;(date-range (date (Date 2018 5 19) (Time 0 0 0 0 0)) 'MO 2 (date (Date 2018 10 23) (Time 0 0 0 0 0)))

;(date-range (current-date) 'MO 100 #f)

; ***********************************************************
;;DateSeries groupby

(define-type Key String)
(define-type GroupHash (HashTable Key (Listof RFDate)))

; This function is self-explanatory, it consumes no arguments
; and creates a hash map which will represent a JoinHash.
(: make-group-hash (-> GroupHash))
(define (make-group-hash)
  (make-hash))

; Used to determine the groups for the groupby. If by is a function, it’s called on each value of the object’s index.
; The Series VALUES will be used to determine the groups if by-value is set to true.
(: date-series-groupby (DateSeries [#:by-value Boolean] -> GroupHash))
(define (date-series-groupby date-series #:by-value [by-value #f])
  (define: group-index : GroupHash (make-group-hash))  

  (let ((len (date-series-length date-series))
        (k (current-continuation-marks)))
    (if (zero? len)
	(raise (make-exn:fail:contract "date-series can't be empty on groupby." k))
	(begin          
	  (do ((i 0 (add1 i)))
	      ((>= i len) group-index)
	    (let* ((date-val : (U RFDate DateSeries) (assert (date-series-iloc date-series (assert i index?)) RFDate?))
                   (date-list : (Listof RFDate) (if (RFDate? date-val) (list date-val) (vector->list (date-series-data date-val))))
                   (key (if by-value
                            (date->string (derive-date-value date-series (assert (date-series-iloc date-series (assert i index?)) RFDate?)))
                            (if (date-series-index date-series)
                                (idx->key (assert (date-series-index date-series)) (assert i index?))
                                (assert i index?))))
                  (key-str : String (cond
                                      [(symbol? key) (symbol->string key)]
                                      [(number? key) (number->string key)]
                                      ; pretty-format anything else
                                      [else (pretty-format key)])))              
              (hash-update! group-index key-str
			      (λ: ((val : (Listof RFDate)))                                
				  (append date-list val))
			      (λ () (list)))))))))

; ***********************************************************

