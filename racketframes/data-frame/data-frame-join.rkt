;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*    Author: Shubham Kahal
;*    File: data-frame-join.rkt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket
(require math/statistics)

; ***********************************************************
; data-frame-join rough draft, currently joins are only possible
; on integer and categorical series
; ***********************************************************

; ***********************************************************
; Provide functions in this file to other files.
(provide:
 [data-frame-join-left (DataFrame DataFrame [#:on (Listof Symbol)] -> DataFrame)]
 [data-frame-join-right (DataFrame DataFrame [#:on (Listof Symbol)] -> DataFrame)]
 [data-frame-join-inner (DataFrame DataFrame [#:on (Listof Symbol)] -> DataFrame)]
 [data-frame-join-outer (DataFrame DataFrame [#:on (Listof Symbol)] -> DataFrame)]
 [data-frame-groupby (DataFrame (Listof Label) -> DataFrameGroupHash)]
 [apply-agg-data-frame (Symbol DataFrameGroupHash -> DataFrame)]
 [copy-column-row-error (Series Integer -> Void)]
 [copy-column-row ((Vectorof Series) (Vectorof SeriesBuilder) Index -> Void)]
 [dest-mapping-series-builders (DataFrameDescription Index -> (Listof SeriesBuilder))]
 [join-column-name (Column (Setof Label) String -> Symbol)]
 [build-multi-index-from-cols ((U (Listof IndexableSeries) Columns) -> SIndex)]
 [key-cols-sort-lexical (Columns -> Columns)]
 [key-cols-series (Columns -> (Listof IndexableSeries))]
 [key-fn ((Listof IndexableSeries) -> (Index -> Key))])

(require
 racket/pretty
 racket/unsafe/ops
 racket/flonum
 racket/set
 (only-in racket/set
          set set-member?
	  list->set set->list
	  set-intersect set-subtract)
 "../util/data-encode.rkt"
 (only-in "../util/symbol.rkt"
          symbol-prefix)
 (only-in "indexed-series.rkt"
	  RFIndex RFNULL RFNoData LabelIndex SIndex Label Labeling LabelProjection
          key-delimiter build-index-from-list ListofIndexDataType?)
 (only-in "series.rkt"
	  series-complete series-data series-iref series-referencer)
 (only-in "series-description.rkt"
	  SeriesType Series Series?
	  SeriesDescription-type
	  series-type series-length          
          IndexableSeries)
 (only-in "data-frame.rkt"
	  DataFrame Column Columns Columns? new-data-frame data-frame-names
	  data-frame-cseries data-frame-explode data-frame-loc data-frame-series-ref
	  DataFrameDescription DataFrameDescription-series data-frame-description)
 (only-in "generic-series.rkt"
	  GenSeries GenSeries? GenericType gen-series-iref new-GenSeries
	  gen-series-referencer)
 (only-in "categorical-series.rkt"
	  cseries-referencer cseries-length cseries-iref
	  CSeries CSeries? new-CSeries)
 (only-in "numeric-series.rkt"
	  NSeries NSeries? nseries-iref new-NSeries
          nseries-referencer NSERIES_DEFAULT_NULL_VALUE)
 (only-in "integer-series.rkt"
	  ISeries ISeries? iseries-iref new-ISeries
	  iseries-referencer RFFixnum ISERIES_DEFAULT_NULL_VALUE)
 (only-in "boolean-series.rkt"
	  BSeries BSeries? bseries-iref new-BSeries
	  bseries-referencer)
 (only-in "datetime-series.rkt"
          DatetimeSeries DatetimeSeries? datetime-series-iref
          new-DatetimeSeries datetime-series-referencer RFDatetime
          DATETIME_SERIES_DEFAULT_NULL_VALUE)
 (only-in "date-series.rkt"
          DateSeries DateSeries? date-series-iref
          new-DateSeries date-series-referencer
          DATE_SERIES_DEFAULT_NULL_VALUE)
 (only-in "../util/datetime/types.rkt"
          Datetime Datetime? Date Time)
 (only-in "series-builder.rkt"
	  SeriesBuilder)
 (only-in "generic-series-builder.rkt"
	  GenSeriesBuilder GenSeriesBuilder?
	  append-GenSeriesBuilder complete-GenSeriesBuilder
	  new-GenSeriesBuilder)
  (only-in "categorical-series-builder.rkt"
	  CSeriesBuilder CSeriesBuilder?
	  append-CSeriesBuilder complete-CSeriesBuilder
	  new-CSeriesBuilder)
 (only-in "categorical-series-ops.rkt"
	  cseries-append)
 (only-in "numeric-series-builder.rkt"
	  NSeriesBuilder NSeriesBuilder?
	  append-NSeriesBuilder complete-NSeriesBuilder
	  new-NSeriesBuilder)
 (only-in "integer-series-builder.rkt"
	  ISeriesBuilder ISeriesBuilder?
	  append-ISeriesBuilder complete-ISeriesBuilder
	  new-ISeriesBuilder)
 (only-in "boolean-series-builder.rkt"
	  BSeriesBuilder BSeriesBuilder?
	  append-BSeriesBuilder complete-BSeriesBuilder
	  new-BSeriesBuilder)
 (only-in "datetime-series-builder.rkt"
	  DatetimeSeriesBuilder DatetimeSeriesBuilder?
	  append-DatetimeSeriesBuilder complete-DatetimeSeriesBuilder
	  new-DatetimeSeriesBuilder)
 (only-in "date-series-builder.rkt"
	  DateSeriesBuilder DateSeriesBuilder?
	  append-DateSeriesBuilder complete-DateSeriesBuilder
	  new-DateSeriesBuilder)
 (only-in "data-frame-print.rkt"
          data-frame-write-delim)
 (only-in "groupby-util.rkt"
          make-agg-value-hash-sindex agg-value-hash-to-gen-series GroupHash
          AggValueHash make-agg-value-hash do-agg))

; ***********************************************************

; ***********************************************************

(define-type Key String)
(define-type JoinHash (HashTable Key (Listof Index)))

(define-predicate ListofReal? (Listof Real))

; ***********************************************************

; ***********************************************************

; This function consumes a Column and returns the series of
; the Column which is just the second element of the list.
(: column-series (Column -> Series))
(define (column-series scol)
  (cdr scol))

(: column-name (Column -> String))
(define (column-name scol)
  (symbol->string (car scol)))

; This function consumes a Column, Setof Label and String
; and checks if the column name of Column is a member of the
; given Setof Label, and if it is, it prepends the prefix
; to the column name and returns that new value. Used for
; join column names.
(: join-column-name (Column (Setof Label) String -> Symbol))
(define (join-column-name column common-cols prefix)
  (let ((colname (car column)))
    (if (set-member? common-cols colname)
	(symbol-prefix colname prefix)
	colname)))

; ***********************************************************

; ***********************************************************

; This function consumes a DataFrameDescription and an Index
; and returns new default series builders of the the given
; length. There will be as many series as there are in the
; DataFrameDecsription.
(: dest-mapping-series-builders (DataFrameDescription Index -> (Listof SeriesBuilder)))
(define (dest-mapping-series-builders data-frame-description len)
  (for/list: : (Listof SeriesBuilder)
	     ([series (DataFrameDescription-series data-frame-description)])
	     (case (SeriesDescription-type series)
               ((GenericSeries)     (new-GenSeriesBuilder len))
	       ((CategoricalSeries) (new-CSeriesBuilder len))
	       ((NumericSeries)     (new-NSeriesBuilder len))
	       ((IntegerSeries)     (new-ISeriesBuilder len))
               ((BooleanSeries)     (new-BSeriesBuilder len))
               ((DatetimeSeries)    (new-DatetimeSeriesBuilder len))
               ((DateSeries)    (new-DateSeriesBuilder len))
	       (else (error 'dest-mapping-series-builders
			    "Unknown series type ~a."
			    (SeriesDescription-type series))))))

; ***********************************************************

; ***********************************************************

; This function consumes a Listof Columns and alphabetically
; sorts it on the column name and returns new sorted list.
(: key-cols-sort-lexical (Columns -> Columns))
(define (key-cols-sort-lexical cols)
  ((inst sort Column Column)
   cols
   (λ: ((kc1 : Column) (kc2 : Column))
       (string<=? (symbol->string (car kc1))
		  (symbol->string (car kc2))))))

; Get indexable Series from Columns
(: key-cols-series (Columns -> (Listof IndexableSeries)))
(define (key-cols-series cols)
  (filter (λ: ((s : Series)) (or (GenSeries? s)
                              (CSeries? s)
                              (ISeries? s)
                              (NSeries? s)
                              (DatetimeSeries? s)
                              (DateSeries? s)))
	  (map column-series cols)))

; Get indexable Series names from Columns
(: col-series-names (Columns -> (Listof String)))
(define (col-series-names cols)
  (map column-name
       (filter (λ: ((col : Column))
                 (let: ((s : Series (cdr col)))
                   (or (GenSeries? s)
                       (CSeries? s)
                       (ISeries? s)
                       (NSeries? s)
                       (DatetimeSeries? s)
                       (DateSeries? s))))
               cols)))

; This function consumes a Listof IndexableSeries and builds key
; string from the columns of a frame and a given set of col labels to use.
; Insert a tab char between each key value, e.g., k1 + \t + k2 + \t + ...
(: key-fn ((Listof IndexableSeries) -> (Index -> Key)))
(define (key-fn cols)
  (let: ((col-refs : (Listof (Index -> GenericType))
		   (for/list ([col (in-list cols)])
                     (series-referencer col)))
         (idx-counter : Index 0))
	(λ: ((row-id : Index))
	    (let ((outp (open-output-string)))
	      (for ([col-ref (in-list col-refs)])
		   (let*: ((seg : GenericType (col-ref row-id))
			   (seg-str : String (cond
                                               [(symbol? seg) (symbol->string seg)]
                                               [(number? seg) (number->string seg)]
                                               ; pretty-format anything else
                                               [else (pretty-format seg)])))
                     (display seg-str outp)
                     (set! idx-counter (assert (add1 idx-counter) index?))
                     (unless (>= idx-counter (length col-refs))
                       (display key-delimiter outp))))
              (get-output-string outp)))))

; ***********************************************************

; ***********************************************************

; This function is self-explanatory, it consumes no arguments
; and creates a hash map which will represent a JoinHash.
(: make-index (-> JoinHash))
(define (make-index)
  (make-hash))

; This function consumes a Listof IndexableSeries and creates
; a JoinHash.
(: index ((Listof IndexableSeries) -> JoinHash))
(define (index cols)

  (define: index : JoinHash (make-index))

  ; Get length of one of the IndexableSeries
  (define len (series-length (car cols)))
  (define: series-key : (Index -> String) (key-fn cols))

  (let loop ([i 0])
    (if (unsafe-fx>= i len)
	index
	(let: ((i : Index (assert i index?)))
	      (let ((key (series-key i)))
		(hash-update! index key
			      (λ: ((idx : (Listof Index)))
				  (cons i idx))
			      (λ () (list))))
	      (loop (add1 i))))))

; ***********************************************************

; ***********************************************************

; This function consumes a CSeries and a CSeriesBuilder and
; returns a function which consumes an index which indexes into
; the CSeries and retrieves the item to append onto the
; CSeriesBuilder.
(: cseries-copy-fn (CSeries CSeriesBuilder -> (Index -> Void)))
(define (cseries-copy-fn series builder)
  (let ((cseries-ref (cseries-referencer series)))
    (λ: ((i : Index))
	(append-CSeriesBuilder builder (cseries-ref i)))))

; This function consumes an ISeries and a ISeriesBuilder and
; returns a function which consumes an index which indexes into
; the ISeries and retrieves the item to append onto the
; ISeriesBuilder.
(: iseries-copy-fn (ISeries ISeriesBuilder -> (Index -> Void)))
(define (iseries-copy-fn series builder)
  (let ((iseries-ref (iseries-referencer series)))
    (λ: ((i : Index))
	(append-ISeriesBuilder builder (iseries-ref i)))))

; This function consumes an NSeries and a NSeriesBuilder and
; returns a function which consumes an index which indexes into
; the NSeries and retrieves the item to append onto the
; NSeriesBuilder.
(: nseries-copy-fn (NSeries NSeriesBuilder -> (Index -> Void)))
(define (nseries-copy-fn series builder)
  (let ((nseries-ref (nseries-referencer series)))
    (λ: ((i : Index))
	(append-NSeriesBuilder builder (nseries-ref i)))))

; This function consumes an ISeries and a ISeriesBuilder and
; returns a function which consumes an index which indexes into
; the ISeries and retrieves the item to append onto the
; ISeriesBuilder.
(: gen-series-copy-fn (GenSeries GenSeriesBuilder -> (Index -> Void)))
(define (gen-series-copy-fn series builder)
  (let ((gen-series-ref (gen-series-referencer series)))
    (λ: ((i : Index))
	(append-GenSeriesBuilder builder (gen-series-ref i)))))

; This function consumes an BSeries and a BSeriesBuilder and
; returns a function which consumes an index which indexes into
; the BSeries and retrieves the item to append onto the
; BSeriesBuilder.
(: bseries-copy-fn (BSeries BSeriesBuilder -> (Index -> Void)))
(define (bseries-copy-fn series builder)
  (let ((bseries-ref (bseries-referencer series)))
    (λ: ((i : Index))
	(append-BSeriesBuilder builder (bseries-ref i)))))

; This function is self explanatory, returns a formated error
; on a copy column row error.
(: copy-column-row-error (Series Integer -> Void))
(define (copy-column-row-error series col)
  (error 'data-frame-join "Invalid target builder for data-frame column series ~s at ~s"
	 (series-type series) col))

; This functions consumes a Vectorof Series and Vectorof SeriesBuilder
; and an Index and does not return any value. It copies an entire row
; from the given Vectorof Series into the given Vectorof SeriesBuilders.
(: copy-column-row ((Vectorof Series) (Vectorof SeriesBuilder) Index -> Void))
(define (copy-column-row src-series dest-builders row-id)
  (for ([col (in-range (vector-length src-series))])
    ; Loop through each column and get the associated series and series builder.
       (let ((series (vector-ref src-series col))
	     (builder (vector-ref dest-builders col)))
         ; Copy specific row values into correct series builders. If series is
         ; a NSeries then associated value will be appended onto NSeriesBuilder,
         ; and same goes for ISeries and CSeries.
         (cond
           ((GenSeries? series)
            (let: ((val : GenericType (gen-series-iref series (list row-id))))
              (if (GenSeriesBuilder? builder)
                  (append-GenSeriesBuilder builder (car (assert val list?)))
                  (copy-column-row-error series col))))
           ((NSeries? series)
            (let: ((num : Flonum (car (nseries-iref series (list row-id)))))
              (if (NSeriesBuilder? builder)
                  (append-NSeriesBuilder builder num)
                  (copy-column-row-error series col))))
           ((CSeries? series)
            (let: ((nom : Label (car (cseries-iref series (list row-id)))))
              (if (CSeriesBuilder? builder)
                  (append-CSeriesBuilder builder nom)
                  (copy-column-row-error series col))))
           ((ISeries? series)
            (let: ((num : RFFixnum (car (iseries-iref series (list row-id)))))
              (if (ISeriesBuilder? builder)
                  (append-ISeriesBuilder builder num)
                  (copy-column-row-error series col))))
           ((BSeries? series)
            (let: ((bool : Boolean (car (bseries-iref series (list row-id)))))
              (if (BSeriesBuilder? builder)
                  (append-BSeriesBuilder builder bool)
                  (copy-column-row-error series col))))
           ((DatetimeSeries? series)
            (let: ((dt : RFDatetime (car (datetime-series-iref series (list row-id)))))
              (if (DatetimeSeriesBuilder? builder)
                  (append-DatetimeSeriesBuilder builder dt)
                  (copy-column-row-error series col))))))))

; This functions consumes a Vectorof Series and Vectorof SeriesBuilder
; and an Index and does not return any value. It copies an entire row
; from the given Vectorof Series into the given Vectorof SeriesBuilders.
(: copy-null-to-row ((Vectorof Series) (Vectorof SeriesBuilder) -> Void))
(define (copy-null-to-row src-series dest-builders)
  (for ([col (in-range (vector-length src-series))])
    ; Loop through each column and get the associated series and series builder.
       (let ((series (vector-ref src-series col))
	     (builder (vector-ref dest-builders col)))
         ; Copy specific row values into correct series builders. If series is
         ; a NSeries then associated value will be appended onto NSeriesBuilder,
         ; and same goes for ISeries and CSeries.
         (cond
           ((GenSeries? series)
            (if (GenSeriesBuilder? builder)
                (append-GenSeriesBuilder builder 'null)
                (copy-column-row-error series col)))
           ((CSeries? series)
            (if (CSeriesBuilder? builder)
                (append-CSeriesBuilder builder 'null)
                (copy-column-row-error series col)))
           ((NSeries? series)
            (if (NSeriesBuilder? builder)
                (append-NSeriesBuilder builder +nan.0)
                (copy-column-row-error series col)))
           ((ISeries? series)
            (if (ISeriesBuilder? builder)
                (append-ISeriesBuilder builder 0)
                (copy-column-row-error series col)))           
           ((BSeries? series)
            (if (BSeriesBuilder? builder)
                (append-BSeriesBuilder builder #f)
                (copy-column-row-error series col)))
           ((DatetimeSeries? series)
            (if (DatetimeSeriesBuilder? builder)
                (append-DatetimeSeriesBuilder builder (Datetime (Date 0 0 0) (Time 0 0 0 0 0)))
                (copy-column-row-error series col)))))))

; ***********************************************************

; ***********************************************************

; This function consumes two Vectorof Series and two Vectorof
; SeriesBuilder. The types of Series and SeriesBuilder must
; match in the respective indicies.
(: do-join-build-left/right ((Vectorof Series) (Vectorof Series) (Vectorof Series)
		  (Vectorof SeriesBuilder) (Vectorof SeriesBuilder)
		  (Index -> Key) JoinHash -> Void))
(define (do-join-build-left/right a-cols b-cols b-cols-match a-builders b-builders dfa-key-fn join-hash)

  (define: a-col-cnt : Fixnum (vector-length a-cols))
  (define: b-col-cnt : Fixnum (vector-length b-cols))
  (define: dfa-len   : Fixnum (series-length (vector-ref a-cols #{0 : Index} )))

  (for ((dfa-row (in-range dfa-len)))
       (let*: ((dfa-row : Index (assert dfa-row index?))
	       (dfa-key : Key (dfa-key-fn dfa-row)))
	      (let ((dfb-rows (hash-ref join-hash dfa-key (λ () '()))))
                ;(displayln (format "Hash join: ~s ~s, ~s" dfa-row dfa-key dfb-rows))
                (if (null? dfb-rows)
                    (begin (copy-column-row a-cols a-builders dfa-row)
                    ; Copy nans into fb
                    (copy-null-to-row b-cols b-builders))
                    ;(copy-null-to-row b-cols-match b-builders))
                    (for ([dfb-row dfb-rows])
                      ; maps possible multiple rows from b to row in a
                      (copy-column-row a-cols a-builders dfa-row)
                      (copy-column-row b-cols b-builders (assert dfb-row index?))))))))

; ***********************************************************

; ***********************************************************

; This function consumes two Vectorof Series and two Vectorof
; SeriesBuilder. The types of Series and SeriesBuilder must
; match in the respective indicies.
(: do-join-build-inner ((Vectorof Series) (Vectorof Series)
		  (Vectorof SeriesBuilder) (Vectorof SeriesBuilder)
		  (Index -> Key) JoinHash -> Void))
(define (do-join-build-inner a-cols b-cols a-builders b-builders dfa-key-fn join-hash)

  (define: a-col-cnt : Fixnum (vector-length a-cols))
  (define: b-col-cnt : Fixnum (vector-length b-cols))
  (define: dfa-len   : Fixnum (series-length (vector-ref a-cols #{0 : Index} )))

  (for ((dfa-row (in-range dfa-len)))
       (let*: ((dfa-row : Index (assert dfa-row index?))
	       (dfa-key : Key (dfa-key-fn dfa-row)))
	      (let ((dfb-rows (hash-ref join-hash dfa-key (λ () '()))))                
                (for ([dfb-row dfb-rows])
                  ; maps possible multiple rows from b to row in a
                  (copy-column-row a-cols a-builders dfa-row)
                  (copy-column-row b-cols b-builders (assert dfb-row index?)))))))

; ***********************************************************

; ***********************************************************

; This function consumes two Vectorof Series and two Vectorof
; SeriesBuilder. The types of Series and SeriesBuilder must
; match in the respective indicies.
(: do-join-build-outer ((Vectorof Series) (Vectorof Series)
		  (Vectorof SeriesBuilder) (Vectorof SeriesBuilder)
		  (Index -> Key) (Index -> Key) JoinHash JoinHash -> Void))
(define (do-join-build-outer a-cols b-cols a-builders b-builders dfa-key-fn dfb-key-fn join-hash-a join-hash-b)

  (define: a-col-cnt : Fixnum (vector-length a-cols))
  (define: b-col-cnt : Fixnum (vector-length b-cols))
  (define: dfa-len   : Fixnum (series-length (vector-ref a-cols #{0 : Index} )))
  (define: dfb-len   : Fixnum (series-length (vector-ref b-cols #{0 : Index} )))

  (define: joined-key-set : (Setof Key) (set))

  ; do for a
  (for ((dfa-row (in-range dfa-len)))
       (let*: ((dfa-row : Index (assert dfa-row index?))
               (dfa-key : Key (dfa-key-fn dfa-row)))
         
         (set! joined-key-set (set-add joined-key-set dfa-key))
         (let ((dfb-rows (hash-ref join-hash-b dfa-key (λ () '()))))
           ;(displayln (format "Hash join A: ~s ~s, ~s" dfa-row dfa-key dfb-rows))
           (if (null? dfb-rows)                    
               (begin
                 ; copy a value but null for b
                 (copy-column-row a-cols a-builders dfa-row)
                 (copy-null-to-row b-cols b-builders))
               (for ([dfb-row dfb-rows])
                 ; maps possible multiple rows from b to row in a                      
                 (copy-column-row a-cols a-builders dfa-row)
                 (copy-column-row b-cols b-builders (assert dfb-row index?)))))))

  ; do vice versa for b
  (for ((dfb-row (in-range dfb-len)))
       (let*: ((dfb-row : Index (assert dfb-row index?))
               (dfb-key : Key (dfb-key-fn dfb-row)))         
         (when (not (subset? (set dfb-key) joined-key-set))
	      (let ((dfa-rows (hash-ref join-hash-a dfb-key (λ () '()))))
                ;(displayln (format "Hash join B: ~s ~s, ~s" dfb-row dfb-key dfa-rows))
                (if (null? dfa-rows)                    
                    (begin
                      ; copy a value but null for b
                      (copy-column-row b-cols b-builders dfb-row)
                      (copy-null-to-row a-cols a-builders))
                    (for ([dfa-row dfa-rows])
                      ; maps possible multiple rows from b to row in a                      
                      (copy-column-row a-cols a-builders (assert dfa-row index?))
                      (copy-column-row b-cols b-builders dfb-row))))))))

; ***********************************************************

; ***********************************************************

; pass in the matched columns as well for display purposes

; This function consumes two DataFrames to join and an optional
; on argument which contains a Listof column names to join on.
; This function does a left join on dfa to dfb.
(: data-frame-join-left (DataFrame DataFrame [#:on (Listof Symbol)] -> DataFrame))
(define (data-frame-join-left dfa dfb #:on [cols '()])

  ; This function consumes a DataFrame and LabelProjection and
  ; projects those columns.
  (: data-frame-cols (DataFrame LabelProjection -> Columns))
  (define (data-frame-cols data-frame project)
    (data-frame-explode data-frame #:project project))

  ; This function consumes a Listof Column and returns a Vectorof
  ; Series contained in those columns.
  (: src-series (Columns -> (Vectorof Series)))
  (define (src-series cols)
    (list->vector (map column-series cols)))

  (define: cols-a    : (Setof Label) (list->set (data-frame-names dfa)))
  (define: cols-b    : (Setof Label) (list->set (data-frame-names dfb)))
  ; Get the common cols between fa and fb
  (define: join-cols : (Setof Label) (if (null? cols)
					 (set-intersect cols-a cols-b)
					 (set-intersect (list->set cols)
							(set-intersect cols-a cols-b))))

  ;(when (null? join-cols)
    ;(error "No common columns between data-frames to join on."))

  ; The column of fb that are not in the join set.
  (define: non-key-dfb : (Setof Label) (set-subtract cols-b join-cols))

  ; get all dfa-cols regardless of join intersection
  (define: dfa-cols : Columns (data-frame-cols dfa '()))
  ; only get dfb-cols not in join intersection
  (define: dfb-cols : Columns (data-frame-cols dfb non-key-dfb))
  ; only get dfb-cols which match for display purposes
  (define: dfb-cols-match : Columns (data-frame-cols dfb (set-intersect cols-a cols-b)))

  ; Create index on fb dataframe on join-cols.
  (define: dfb-index : JoinHash
    (let ((cols (key-cols-sort-lexical (data-frame-cols dfb join-cols))))
      (index (key-cols-series cols))))

  (define: dfa-keyfn : (Index -> Key)
    (key-fn (key-cols-series (key-cols-sort-lexical (data-frame-cols dfa join-cols)))))

  (: base-len Index)
  (define base-len 512)
  
  ; Get series builders of default length 512 for all columns in fa.
  (define: dest-builders-a : (Vectorof SeriesBuilder)
    (list->vector (dest-mapping-series-builders (data-frame-description dfa) base-len)))

  ; Get series builders of default length 512 for only non-key-fb columns in fb.
  (define: dest-builders-b : (Vectorof SeriesBuilder)
    (list->vector
     (dest-mapping-series-builders (data-frame-description dfb #:project non-key-dfb) base-len)))
  
  (do-join-build-left/right (src-series dfa-cols) (src-series dfb-cols) (src-series dfb-cols-match)
		 dest-builders-a dest-builders-b
		 dfa-keyfn dfb-index)

  (define: new-a-series : Columns
    (for/list ([builder (in-vector dest-builders-a)]
	       [col     (in-list dfa-cols)])
	      (cons (join-column-name col cols-a "dfa-")
		    (series-complete builder))))

  (define: new-b-series : Columns
    (for/list ([builder (in-vector dest-builders-b)]
	       [col     (in-list dfb-cols)])
	      (cons (join-column-name col cols-b "dfb-")
		    (series-complete builder))))

  (new-data-frame (append new-a-series new-b-series)))

; ***********************************************************

; ***********************************************************

;; right outer join, just reverse fa and fb operations

; This function consumes two DataFrames to join and an optional
; on argument which contains a Listof column names to join on.
(: data-frame-join-right (DataFrame DataFrame [#:on (Listof Symbol)] -> DataFrame))
(define (data-frame-join-right dfa dfb #:on [cols '()])

  ; This function consumes a DataFrame and LabelProjection and
  ; projects those columns.
  (: data-frame-cols (DataFrame LabelProjection -> Columns))
  (define (data-frame-cols data-frame project)
    (data-frame-explode data-frame #:project project))

  ; This function consumes a Listof Column and returns a Vectorof
  ; Series contained in those columns.
  (: src-series (Columns -> (Vectorof Series)))
  (define (src-series cols)
    (list->vector (map column-series cols)))

  (define: cols-a    : (Setof Label) (list->set (data-frame-names dfa)))
  (define: cols-b    : (Setof Label) (list->set (data-frame-names dfb)))
  ; Get the common cols between fa and fb
  (define: join-cols : (Setof Label) (if (null? cols)
					 (set-intersect cols-b cols-a)
					 (set-intersect (list->set cols)
							(set-intersect cols-b cols-a))))

  ;(when (null? join-cols)
  ;(error "No common columns between data-frames to join on."))

  ; The column of fa that are not in the join set.
  (define: non-key-dfa : (Setof Label) (set-subtract cols-a join-cols))

  ; get all fb-cols regardless of join intersection
  (define: dfb-cols : Columns (data-frame-cols dfb '()))
  ; only get fa-cols not in join intersection
  (define: dfa-cols : Columns (data-frame-cols dfa non-key-dfa))

  ; only get dfb-cols which match for display purposes
  (define: dfa-cols-match : Columns (data-frame-cols dfa (set-intersect cols-a cols-b)))

  ; Create index on dfa dataframe on join-cols.
  (define: dfa-index : JoinHash
    (let ((cols (key-cols-sort-lexical (data-frame-cols dfa join-cols))))
      (index (key-cols-series cols))))

  (define: dfb-keyfn : (Index -> Key)
    (key-fn (key-cols-series (key-cols-sort-lexical (data-frame-cols dfb join-cols)))))

  (: base-len Index)
  (define base-len 512)

  ; Get series builders of default length 512 for all columns in fb.
  (define: dest-builders-b : (Vectorof SeriesBuilder)
    (list->vector (dest-mapping-series-builders (data-frame-description dfb) base-len)))

  ; Get series builders of default length 512 for only non-key-fb columns in fa.
  (define: dest-builders-a : (Vectorof SeriesBuilder)
    (list->vector
     (dest-mapping-series-builders (data-frame-description dfa #:project non-key-dfa) base-len)))

  (do-join-build-left/right (src-series dfb-cols) (src-series dfa-cols) (src-series dfa-cols-match)
		 dest-builders-b dest-builders-a
		 dfb-keyfn dfa-index)

  (define: new-a-series : Columns
    (for/list ([builder (in-vector dest-builders-a)]
	       [col     (in-list dfa-cols)])
	      (cons (join-column-name col cols-a "dfa-")
		    (series-complete builder))))

  (define: new-b-series : Columns
    (for/list ([builder (in-vector dest-builders-b)]
	       [col     (in-list dfb-cols)])
	      (cons (join-column-name col cols-b "dfb-")
		    (series-complete builder))))

  (new-data-frame (append new-a-series new-b-series)))

; ***********************************************************

; ***********************************************************

; This function consumes two DataFrames to join and an optional
; on argument which contains a Listof column names to join on.
; This function does a left join on dfa to dfb.

(: data-frame-join-inner (DataFrame DataFrame [#:on (Listof Symbol)] -> DataFrame))
(define (data-frame-join-inner dfa dfb #:on [cols '()])

  ; This function consumes a DataFrame and LabelProjection and
  ; projects those columns.
  (: data-frame-cols (DataFrame LabelProjection -> Columns))
  (define (data-frame-cols data-frame project)
    (data-frame-explode data-frame #:project project))

  ; This function consumes a Listof Column and returns a Vectorof
  ; Series contained in those columns.
  (: src-series (Columns -> (Vectorof Series)))
  (define (src-series cols)
    (list->vector (map column-series cols)))

  (define: cols-a    : (Setof Label) (list->set (data-frame-names dfa)))
  (define: cols-b    : (Setof Label) (list->set (data-frame-names dfb)))
  ; Get the common cols between fa and fb
  (define: join-cols : (Setof Label) (if (null? cols)
                                         ;(let ((cols-a-b-intersection (set-intersect cols-a cols-b)))
                                          ; (if (or (= (set-count cols-a-b-intersection) (set-count cols-b)) (= (set-count cols-a-b-intersection) (set-count cols-a)))
                                           ;    (set)
                                            ;   cols-a-b-intersection))
                                         (set-intersect cols-b cols-a)
					 (set-intersect (list->set cols)
							(set-intersect cols-a cols-b))))

  ;(displayln "Join Cols")

  ;(displayln join-cols)

  (when (null? join-cols)
	(error "No common columns between data-frames to join on."))

  ;(when (or (= (set-count join-cols) (set-count cols-b)) (= (set-count join-cols) (set-count cols-b)))
   ; (set-clear! join-cols))

  ; The column of fb that are not in the join set.
  (define: non-key-dfb : (Setof Label) (set-subtract cols-b join-cols))

  ; get all dfa-cols regardless of join intersection
  (define: dfa-cols : Columns (data-frame-cols dfa '()))
  ; only get dfb-cols not in join intersection
  (define: dfb-cols : Columns (data-frame-cols dfb '()))

  ; Create index on fb dataframe on join-cols.
  (define: dfb-index : JoinHash
    (let ((cols (key-cols-sort-lexical (data-frame-cols dfb join-cols))))
      (index (key-cols-series cols))))

  (define: dfa-keyfn : (Index -> Key)
    (key-fn (key-cols-series (key-cols-sort-lexical (data-frame-cols dfa join-cols)))))

  (: base-len Index)
  (define base-len 512)
  
  ; Get series builders of default length 512 for all columns in fa.
  (define: dest-builders-a : (Vectorof SeriesBuilder)
    (list->vector (dest-mapping-series-builders (data-frame-description dfa) base-len)))

  ; Get series builders of default length 512 for only non-key-fb columns in fb.
  (define: dest-builders-b : (Vectorof SeriesBuilder)
    (list->vector
     (dest-mapping-series-builders (data-frame-description dfb) base-len)))

  (do-join-build-inner (src-series dfa-cols) (src-series dfb-cols)
		 dest-builders-a dest-builders-b
		 dfa-keyfn dfb-index)

  (define: new-a-series : Columns
    (for/list ([builder (in-vector dest-builders-a)]
	       [col     (in-list dfa-cols)])
	      (cons (join-column-name col cols-a "dfa-")
		    (series-complete builder))))

  (define: new-b-series : Columns
    (for/list ([builder (in-vector dest-builders-b)]
	       [col     (in-list dfb-cols)])
	      (cons (join-column-name col cols-b "dfb-")
		    (series-complete builder))))

  (new-data-frame (append new-a-series new-b-series)))

; ***********************************************************

; ***********************************************************

; This function consumes two DataFrames to join and an optional
; on argument which contains a Listof column names to join on.
; This function does a left join on dfa to dfb.

(: data-frame-join-outer (DataFrame DataFrame [#:on (Listof Symbol)] -> DataFrame))
(define (data-frame-join-outer dfa dfb #:on [cols '()])

  ; This function consumes a DataFrame and LabelProjection and
  ; projects those columns.
  (: data-frame-cols (DataFrame LabelProjection -> Columns))
  (define (data-frame-cols data-frame project)
    (data-frame-explode data-frame #:project project))

  ; This function consumes a Listof Column and returns a Vectorof
  ; Series contained in those columns.
  (: src-series (Columns -> (Vectorof Series)))
  (define (src-series cols)
    (list->vector (map column-series cols)))

  (define: cols-a    : (Setof Label) (list->set (data-frame-names dfa)))
  (define: cols-b    : (Setof Label) (list->set (data-frame-names dfb)))
  ; Get the common cols between fa and fb
  (define: join-cols : (Setof Label) (if (null? cols)
					 (set-intersect cols-a cols-b)
					 (set-intersect (list->set cols)
							(set-intersect cols-a cols-b))))

  ;(when (null? join-cols)
  ;(error "No common columns between data-frames to join on."))

  ; The column of fb that are not in the join set.
  (define: non-key-dfb : (Setof Label) (set-subtract cols-b join-cols))

  ; get all dfa-cols regardless of join intersection
  (define: dfa-cols : Columns (data-frame-cols dfa '()))
  ; only get dfb-cols not in join intersection
  (define: dfb-cols : Columns (data-frame-cols dfb '()))

  ; Create index on fb dataframe on join-cols.
  (define: dfa-index : JoinHash
    (let ((cols (key-cols-sort-lexical (data-frame-cols dfa join-cols))))
      (index (key-cols-series cols))))
  
  ; Create index on fb dataframe on join-cols.
  (define: dfb-index : JoinHash
    (let ((cols (key-cols-sort-lexical (data-frame-cols dfb join-cols))))
      (index (key-cols-series cols))))

  (define: dfa-keyfn : (Index -> Key)
    (key-fn (key-cols-series (key-cols-sort-lexical (data-frame-cols dfa join-cols)))))

  (define: dfb-keyfn : (Index -> Key)
    (key-fn (key-cols-series (key-cols-sort-lexical (data-frame-cols dfb join-cols)))))

  (: base-len Index)
  (define base-len 512)

  ; Get series builders of default length 512 for all columns in fa.
  (define: dest-builders-a : (Vectorof SeriesBuilder)
    (list->vector (dest-mapping-series-builders (data-frame-description dfa) base-len)))

  ; Get series builders of default length 512 for only non-key-fb columns in fb.
  (define: dest-builders-b : (Vectorof SeriesBuilder)
    (list->vector
     (dest-mapping-series-builders (data-frame-description dfb) base-len)))

  (do-join-build-outer (src-series dfa-cols) (src-series dfb-cols)
		 dest-builders-a dest-builders-b
		 dfa-keyfn dfb-keyfn dfa-index dfb-index)

  (define: new-a-series : Columns
    (for/list ([builder (in-vector dest-builders-a)]
	       [col     (in-list dfa-cols)])
	      (cons (join-column-name col cols-a "dfa-")
		    (series-complete builder))))

  (define: new-b-series : Columns
    (for/list ([builder (in-vector dest-builders-b)]
	       [col     (in-list dfb-cols)])
	      (cons (join-column-name col cols-b "dfb-")
		    (series-complete builder))))

  (new-data-frame (append new-a-series new-b-series)))

; ***********************************************************

; ***********************************************************

(define-type DataFrameGroupHash (HashTable Key GroupHash))

; maps DataFrame index to mapping of columns to aggregate result
; {'index_groupby_columns' : {'col':'agg_result', 'col':'agg_result', 'col':'agg_result'} ...}
; the hash-keys will be converted to a RF index while the column data will be organized into
; series.
(define-type DataFrameAggValueHash (Mutable-HashTable Key AggValueHash))

; This function is self-explanatory, it consumes no arguments
; and creates a hash map which will represent a JoinHash.
(: make-group-hash (-> GroupHash))
(define (make-group-hash)
  (make-hash))

(: make-data-frame-group-hash (-> DataFrameGroupHash))
(define (make-data-frame-group-hash)
  (make-hash))

(: make-data-frame-agg-value-hash (-> DataFrameAggValueHash))
(define (make-data-frame-agg-value-hash)
  (make-hash))

;Used to determine the groups for the groupby. The Series VALUES will be used to determine the groups.
(: data-frame-groupby (DataFrame (Listof Label) -> DataFrameGroupHash))
(define (data-frame-groupby data-frame by)
  (define: group-index : DataFrameGroupHash (make-data-frame-group-hash))
  (define: col-groups : (Listof IndexableSeries) (key-cols-series (data-frame-explode data-frame #:project by)))
  (define: group-data-cols : Columns (data-frame-explode data-frame #:project (set-subtract (list->set (data-frame-names data-frame)) (list->set by))))
  (define: col-data : (Listof IndexableSeries) (key-cols-series group-data-cols))
  (define: col-names : (Listof Key) (col-series-names group-data-cols))
  
  ; Get length of one of the IndexableSeries
  (define len (series-length (car col-groups)))
  (define: group-key : (Index -> String) (key-fn col-groups))

  (let loop ([i 0])
    (if (unsafe-fx>= i len)
	group-index
	(let: ((i : Index (assert i index?)))
          (let ((key (group-key i)))
            (hash-update! group-index key
                          (λ: ((col-grouping : GroupHash))
                            (begin 
                              (for ([col-name col-names])                                  
                                (hash-update! col-grouping col-name
                                              (λ: ((col-group-data : (Listof GenericType)))
                                                (flatten (cons (series-iref (assert (data-frame-series-ref data-frame (string->symbol col-name)) Series?) i) col-group-data)))
                                              (λ () (list))))
                              col-grouping))
                          (λ () (make-group-hash))))
          (loop (add1 i))))))

; ***********************************************************

; ***********************************************************
;; DataFrame agg ops

;; DataFrameAggValueHash (HashTable Key AggValueHash)
(: agg-value-hash-to-data-frame (DataFrameAggValueHash -> DataFrame))
(define (agg-value-hash-to-data-frame data-frame-agg-value-hash)
  ; build multi-index if more than one grouping column is given
  (let ((rf-index : RFIndex (build-index-from-list (assert (map (λ (str) (string->symbol str)) (hash-keys data-frame-agg-value-hash)) ListofIndexDataType?))) (col-data-hash : (Mutable-HashTable Label (Listof GenericType)) (make-hash)))
    (begin 
      (hash-for-each data-frame-agg-value-hash
                     (lambda ([index-key : String] [agg-value-hash : AggValueHash])
                       (hash-for-each agg-value-hash
                                      (lambda ([col-name : String] [val : GenericType])
                                        (hash-update! col-data-hash (string->symbol col-name)
                                                      (λ: ((col-data : (Listof GenericType)))
                                                        (cons val col-data))
                                                      (λ () (list)))))))
      
      ;(display col-data-hash)
      
      (new-data-frame col-data-hash #:index rf-index))))

; Applies the aggregate function specificed by function-name to the values in
; the column-name column.
(: apply-agg-data-frame (Symbol DataFrameGroupHash -> DataFrame))
(define (apply-agg-data-frame function-name data-frame-group-hash)
  (define len (hash-count data-frame-group-hash))

  ; DataFrameAggValueHash (HashTable Key (HashTable Key GenericType))
  (: data-frame-agg-value-hash DataFrameAggValueHash)
  (define data-frame-agg-value-hash (make-data-frame-agg-value-hash))

  (hash-for-each data-frame-group-hash
                 (lambda ([df-index-key : String] [col-group-hash : GroupHash])
                   (let ((agg-value-hash (make-agg-value-hash)))
                     (begin 
                       (hash-for-each col-group-hash
                                      (lambda ([key : String] [col-data : (Listof GenericType)])
                                        (let ((key (assert key string?))
                                              (col-data (flatten col-data)))
                                          (hash-set! agg-value-hash key 
                                                     ; need object with df-index-key pointing to {df-index-key -> {key -> result}}
                                                     ; only operate on numeric series for mathematical aggregate functions
                                                     (do-agg function-name col-data)))))
                       (hash-set! data-frame-agg-value-hash df-index-key agg-value-hash)))))
  (agg-value-hash-to-data-frame data-frame-agg-value-hash))

; **********************************************************

; ***********************************************************

(: build-multi-index-from-cols ((U (Listof IndexableSeries) Columns) -> SIndex))
(define (build-multi-index-from-cols cols)

  (let ((cols : (Listof IndexableSeries)
              (if (Columns? cols)
                  (key-cols-series cols)
                  cols)))

    ; Get length of one of the IndexableSeries
    (define len (series-length (car cols)))
    (define: series-key : (Index -> String) (key-fn cols))

    (let ((index : SIndex (make-hash '())))
      (let loop ([i 0])
        (if (>= i len)
            index
            (let: ((i : Index (assert i index?)))
              (let ((key (series-key i)))
                (hash-update! index (string->symbol key)
                              (λ: ((idx : (Listof Index)))
                                (append idx (list i)))
                              (λ () (list))))
              (loop (add1 i))))))))

; **********************************************************
