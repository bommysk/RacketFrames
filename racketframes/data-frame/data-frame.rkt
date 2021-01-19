;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*    Author: Shubham Kahal
;*    File: data-frame.rkt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket

; ***********************************************************
; A map of series to label names, represented as a collection
; of columns.
; ***********************************************************

; ***********************************************************
; Provide functions in this file to other files.

(provide:
 [column (Label Series -> Column)]
 [column-heading (Column -> Label)]
 [column-series (Column -> Series)]
 [data-frame-rename (DataFrame Label Label -> DataFrame)]
 [data-frame-project (DataFrame LabelProjection -> DataFrame)]
 [data-frame-drop (DataFrame Label -> DataFrame)]
 [data-frame-remove (DataFrame LabelProjection -> DataFrame)]
 [data-frame-explode (DataFrame [#:project LabelProjection] -> Columns)]
 [data-frame-replace (DataFrame Column -> DataFrame)]
 [data-frame-extend  (DataFrame (U Column Columns DataFrame) -> DataFrame)]
 [data-frame-description (DataFrame [#:project LabelProjection] -> DataFrameDescription)]
 [show-data-frame-description (DataFrameDescription -> Void)]
 [data-frame-set-index (DataFrame (U (Listof Label) (Listof Fixnum) (Listof Flonum) (Listof Datetime) RFIndex) -> DataFrame)]
 [data-frame-loc (DataFrame (U Label (Listof Label) (Listof Boolean)) LabelProjection -> (U Series DataFrame))]
 [data-frame-iloc (DataFrame (U Index (Listof Index)) (U Index (Listof Index)) -> (U Series DataFrame))]
 [data-frame-iloc-label (DataFrame (U Index (Listof Index)) LabelProjection -> (U Series DataFrame))]
 [data-frame-labels (DataFrame -> (Listof (Pair Symbol (Listof Index))))]
 [projection-set (LabelProjection -> (Setof Label))]
 [data-frame-all-labels-projection-set (DataFrame -> (Setof Label))])

(provide
 DataFrame DataFrame? Column Column? Columns Columns?
 (struct-out DataFrameDescription)
 data-frame-series
 data-frame-names data-frame-dim 
 data-frame-cseries data-frame-nseries data-frame-iseries
 new-data-frame)

; ***********************************************************

; ***********************************************************

(require
 racket/flonum
 racket/set
 racket/list
 racket/sequence
 (only-in racket/vector
          vector-empty?
	  vector-copy)
 (only-in racket/set
	  set-empty? set-member? set-subtract
	  list->set)
 (only-in "types.rkt"
          Dim Dim-rows Dim-cols)
 (only-in "indexed-series.rkt"
	  RFIndex label-sort-positional ListofLabel? LabelIndex?
          Label LabelProjection LabelProjection? LabelIndex LabelIndex-index
          build-index-from-labels build-index-from-list label-index idx->key
          idx->label)
 (only-in "series-description.rkt"
	  series-description series-length series-type series-data
          Series Series?
          SeriesDescription SeriesDescription-name
          SeriesDescription-type SeriesDescription-length
          set-series-index series-loc-boolean series-loc series-iloc)
 (only-in "generic-series.rkt"
         GenSeries GenericType GenSeries?
         GenSeries-data
         new-GenSeries)
 (only-in "categorical-series.rkt"
          CSeries CSeries?
          CSeries-data
          new-CSeries)
 (only-in "numeric-series.rkt"
          NSeries NSeries? 
          NSeries-data
          new-NSeries)
 (only-in "integer-series.rkt"
	  ISeries ISeries?
	  ISeries-data
	  new-ISeries)
 (only-in "boolean-series.rkt"
	  BSeries BSeries?
	  BSeries-data
	  new-BSeries)
 (only-in  "../util/datetime.rkt"
           Datetime))

; ***********************************************************

; ***********************************************************
; Data Structure Definitions

(define-type Column (Pair Label Series))
(define-type Columns (Listof Column))

(define-predicate Column? Column)

(define-predicate Columns? Columns)

;; A DataFrame is map of series.
(struct: DataFrame LabelIndex ([series : (Vectorof Series)]))

(struct: DataFrameDescription ([dimensions : Dim]
                               [series : (Listof SeriesDescription)]))

; ***********************************************************

; ***********************************************************

; This function consumes a Label and Series and defines a
; Column;
(: column (Label Series -> Column))
(define (column label series)
  (cons label series))

; This function consumes a Column and just retrieves the
; first element of pair which is the label heading.
(: column-heading (Column -> Label))
(define (column-heading col)
  (car col))

; This function consumes a Column and just retrieves the
; second element of pair which is the actual series.
(: column-series (Column -> Series))
(define (column-series col)
  (cdr col))

; ***********************************************************

(: seq->columns ((U Columns (Sequenceof Label (Sequenceof Any))) -> Columns))
(define (seq->columns col/seq)
  (if (Columns? col/seq)
      col/seq
      (let: ((hash : (HashTable Label (Listof Any)) (make-hash))
             (cols : Columns '()))
        (begin
          (for ([([k : Label] [v : (Sequenceof Any)]) col/seq])              
            (let*: ((h-ref : (Listof Any) (hash-ref hash k (λ () '())))
                    (v-value : (Listof Any)
                             (if (list? v)
                                 v
                                 (if (vector? v)
                                     (vector->list v)
                                     (if (sequence? v)
                                         (sequence->list v)
                                         (list v))))))
              
              (hash-set! hash k (append v-value h-ref))))
        
        
          (hash-for-each hash
                         (lambda ([k : Label] [v : Any])
                           (let: ((h-ref : (Listof Any) (hash-ref hash k (λ () '()))))
                             (set! cols (cons (cons k (new-GenSeries (list->vector h-ref) #f)) cols))))))
        
        cols)))

; ***********************************************************

; This function consumes a Listof Column and constructs a
; DataFrame from it. The function checks that all columns
; are of the same length and builds a LabelIndex and a
; Vectorof Series.
(: new-data-frame ((U Columns (Sequenceof Label (Sequenceof Any))) -> DataFrame))
(define (new-data-frame cols)

  (: cols/seq->columns Columns)
  (define cols/seq->columns (seq->columns cols))
  
  (define (check-equal-length)
    (when  (pair? cols/seq->columns)
	   (let ((len (if (null? cols/seq->columns) 
			  0 
			  (series-length (cdr (car cols/seq->columns))))))
             (unless (andmap (λ: ((s : (Pair Symbol Series)))
                               (eq? len (series-length (cdr s))))
                             (cdr cols/seq->columns))
               (error 'new-data-frame "Frame must have equal length series: ~a" 
                      (map (λ: ((s : (Pair Symbol Series)))
                             (series-description (car s) (cdr s)))
                           cols/seq->columns))))))

  (: are-all-unique? ((Listof Symbol) -> Boolean))
  (define are-all-unique?
    (lambda (v)
      (if (pair? v)
          (and (not (member (car v) (cdr v)))
               (are-all-unique? (cdr v)))
          #t)))
  
  (check-equal-length)

  (when (not (are-all-unique? (map (λ: ((s : (Pair Symbol Series)))
                                     (car s)) cols/seq->columns)))
    (error 'new-data-frame "Frame must have uniquely named series: ~a" (map (λ: ((s : (Pair Symbol Series)))
                                                                              (series-description (car s) (cdr s)))
                                                                            cols/seq->columns)))
    
  
  (let ((index (build-index-from-labels ((inst map Label Column)
                                         (inst car Label Series) (assert cols/seq->columns Columns?))))
        (data (apply vector ((inst map Series Column) cdr cols/seq->columns))))
    (DataFrame index data)))

; ***********************************************************

; ***********************************************************

; This function consumes a DataFrame and two labels. It locates
; the index of the column in the DataFrame LabelIndex and creates
; a new LabelIndex with the new column name mapped to this index.
; Then a new DataFrame is created and returned.
(: data-frame-rename (DataFrame Label Label -> DataFrame))
(define (data-frame-rename data-frame from to)
  (let ((index (LabelIndex-index data-frame)))
    (if index
	(let: ((col-idx : (Option (Listof Index)) (hash-ref index from (λ () #f))))
	      (if col-idx
		  (let ((new-index (hash-copy index)))
		    (hash-remove! new-index from)
		    (hash-set! new-index to col-idx)
		    (DataFrame new-index (vector-copy (DataFrame-series data-frame))))
		  data-frame))
	data-frame)))

; ***********************************************************

; ***********************************************************

; This function consumes a DataFrame and a Label for the column
; to drop from the DataFrame. Using the data-frame-explode
; function, this column is filtered out of the DataFrame, and a
; new DataFrame is returned.
(: data-frame-drop (DataFrame Label -> DataFrame))
(define (data-frame-drop data-frame label)
  (new-data-frame (filter (λ: ((col : Column))
			 (not (eq? (car col) label)))
		     (data-frame-explode data-frame))))

; ***********************************************************

; ***********************************************************
; This function consumes a DataFrame and a Symbol representing
; the column name and returns the Series of that column.
(: data-frame-series (DataFrame Symbol -> Series))
(define (data-frame-series data-frame col)
  (vector-ref (DataFrame-series data-frame)
              (car (label-index (assert (LabelIndex-index data-frame)) col))))

; This function uses the above function and the assert function
; to ensure the series returned is a GenSeries.
(: data-frame-gen-series (DataFrame Symbol -> GenSeries))
(define (data-frame-gen-series data-frame name)
  (assert (data-frame-gen-series data-frame name) GenSeries?))

; This function uses the above function and the assert function
; to ensure the series returned is a CSeries.
(: data-frame-cseries (DataFrame Symbol -> CSeries))
(define (data-frame-cseries data-frame name)
  (assert (data-frame-series data-frame name) CSeries?))

; This function uses the above function and the assert function
; to ensure the series returned is a NSeries.
(: data-frame-nseries (DataFrame Symbol -> NSeries))
(define (data-frame-nseries data-frame name)
  (assert (data-frame-series data-frame name) NSeries?))

; This function uses the above function and the assert function
; to ensure the series returned is an ISeries.
(: data-frame-iseries (DataFrame Symbol -> ISeries))
(define (data-frame-iseries data-frame name)
  (assert (data-frame-series data-frame name) ISeries?))

; This function uses the above function and the assert function
; to ensure the series returned is an BSeries.
(: data-frame-bseries (DataFrame Symbol -> BSeries))
(define (data-frame-bseries data-frame name)
  (assert (data-frame-series data-frame name) BSeries?))

; This function consumes a DataFrame and returns a Listof pairs
; consisting of the column name and its associated indices in the
; DataFrame.
(: data-frame-labels (DataFrame -> (Listof (Pair Symbol (Listof Index)))))
(define (data-frame-labels data-frame)
  (hash->list (assert (LabelIndex-index data-frame))))

; ***********************************************************

; ***********************************************************

; (inst) example usage
; (map (inst cons Symbol Fixnum) '(a b c d) '(1 2 3 4))
; - : (Listof (Pairof Symbol Fixnum))
; '((a . 1) (b . 2) (c . 3) (d . 4))

; This function consumes a DataFrame and returns of Listof
; Symbol representing all the column names.
(: data-frame-names (DataFrame -> (Listof Symbol)))
(define (data-frame-names data-frame)  
  (map (λ: ((kv : (Pair Symbol (Listof Fixnum))))
	   (car kv))
       ((inst sort (Pair Symbol (Listof Index)) (Pair Symbol (Listof Index)))
        (data-frame-labels data-frame)
        (λ: ((kv1 : (Pair Symbol (Listof Index)))
             (kv2 : (Pair Symbol (Listof Index))))
	    (< (car (cdr kv1)) (car (cdr kv2)))))))

; ***********************************************************

; ***********************************************************

; This function consumes a DataFrame and calculates the dimensions.
; It does so by calculating the length of a column series, thus
; getting the number of rows and the total number of columns. It
; then stores these two values in the Dim struct and returns it.
(: data-frame-dim (DataFrame -> Dim))
(define (data-frame-dim data-frame)
  (let ((cols (length (hash-keys (assert (LabelIndex-index data-frame))))))
    (if (zero? cols)
        (Dim 0 0)
        (let ((rows (let ((series (vector-ref (DataFrame-series data-frame) 0)))
                      (series-length series))))                      
          (Dim rows cols)))))

; ***********************************************************

; ***********************************************************
; This function consumes a LabelProjection which may be a
; Listof Label or a Setof Label and ensure that it is a Setof
; Label and returns this set.
(: projection-set (LabelProjection -> (Setof Label)))
(define (projection-set project)
  (if (list? project) 
      (list->set project) 
      project))

; This function consumes a DataFrame and using the project-set
; function above returns a sorted set of all the column names
; in the DataFrame.
(: data-frame-all-labels-projection-set (DataFrame -> (Setof Label)))
(define (data-frame-all-labels-projection-set data-frame)
  (projection-set (map (inst car Symbol Any) (label-sort-positional data-frame))))

; This function consumes a List of type A and a function which
; consumes an element of type A and returns a Symbol as well as
; a LabelProjection which is a Listof Label or Setof Label and
; returns a Listof A which is filtered based on the labels present
; in the given LabelProjection. Thus only the items that are desired
; to be projected are projected.
(: projection-filter (All (A) (Listof A) (A -> Symbol) LabelProjection -> (Listof A)))
(define (projection-filter lst sym-fn project)  
  (define projection (projection-set project))  
  (if (set-empty? projection)
      lst
      (filter (λ: ((a : A))
		  (set-member? projection (sym-fn a)))
	      lst)))

; ***********************************************************

; ***********************************************************

; This function consumes a DataFrame and an optional LabelProjection
; adn constructs a DataFrameDescription struct which contains the
; DataFrame Dim and a list of SeriesDecriptions for each series of
; each column.
(: data-frame-description (DataFrame [#:project LabelProjection] -> DataFrameDescription))
(define (data-frame-description data-frame #:project [project '()])
  
  (let ((names (data-frame-names data-frame)))
    (let: loop : DataFrameDescription ((names : (Listof Label) names) 
				   (descs : (Listof SeriesDescription) '()))
	  (if (null? names)
	      (let ((dim (data-frame-dim data-frame))
		    (cols (projection-filter descs 
					     (λ: ((sd : SeriesDescription))
						 (SeriesDescription-name sd))
					     project)))
		(DataFrameDescription (Dim (Dim-rows dim)
				       (length cols))
				  (reverse cols)))
	      (let* ((name (car names))
		     (series (data-frame-series data-frame name)))
		(loop (cdr names) (cons (SeriesDescription name 
							   (series-type series) 
							   (series-length series))
					descs)))))))

; This function consumes a DataFrameDescription struct and displays
; the DataFrame Dim and Series Descriptions in formated form.
(: show-data-frame-description (DataFrameDescription -> Void))
(define (show-data-frame-description fdesc)
  
  (: print-series-description (SeriesDescription -> Void))
  (define (print-series-description sdesc)
    (displayln (format "  - ~a: ~a"
		       (SeriesDescription-name sdesc)
		       (SeriesDescription-type sdesc))))                      
  
  (let ((dim (DataFrameDescription-dimensions fdesc)))
    (displayln (format "DataFrame::(Cols: ~a, Rows: ~a)" (Dim-cols dim) (Dim-rows dim)))
    (for-each print-series-description (DataFrameDescription-series fdesc))))

; ***********************************************************

; ***********************************************************

; This function consumes a DataFrame and an optional LabelProjection
; which indicates which columns to project and returns a Listof Column.
; If no LabelProjection is given, all columns are projected. The
; label-sort-positional returns a labeling, which is a (Listof (Pair Label Index)).
; This list is looped through and only the items to project are filtered for.
; In the end an appropriate Listof Column is returned.
(: data-frame-explode (DataFrame [#:project LabelProjection] -> Columns))
(define (data-frame-explode data-frame #:project [project '()])  
  (let ((labeling (label-sort-positional data-frame))
	(series (DataFrame-series data-frame)))
    (projection-filter (for/list: : Columns
				  ([label labeling])
				  (cons (car label)
					(vector-ref series (car (cdr label)))))
		       (λ: ((l-s : Column))
			   (car l-s))
		       project)))

; ***********************************************************

; ***********************************************************

; This function consumes a DataFrame and LabelProjection indicating
; which columns to remove from the DataFrame. The column names in
; the DataFrame are put into a set called all-labels. The drop-projection
; is converted to a set as well. The labels to be kept are calculated
; by substracting the drop-projection from all of the possible labels.
; Then the DataFrame is exploded retaining only the columns to keep.
; A new data frame is constructed fomr these column and returned.
(: data-frame-remove (DataFrame LabelProjection -> DataFrame))
(define (data-frame-remove data-frame drop-projection)
  (define all-labels (data-frame-all-labels-projection-set data-frame))
  (define drop-labels (projection-set drop-projection))
  (define keep-labels (set-subtract all-labels drop-labels))
  (new-data-frame (data-frame-explode data-frame #:project keep-labels)))

; ***********************************************************

; ***********************************************************

; This function consumes a DataFrame and LabelProjection and
; creates a new DataFrame only containing the columns to project.
(: data-frame-project (DataFrame LabelProjection -> DataFrame))
(define (data-frame-project data-frame projection)
  (new-data-frame (data-frame-explode data-frame #:project (projection-set projection))))

; ***********************************************************

; ***********************************************************

; This function consumes a DataFrame and Column and replaces
; the old column with the same column heading with this new
; column.
(: data-frame-replace (DataFrame Column -> DataFrame))
(define (data-frame-replace data-frame new-col)
  (define name (column-heading new-col))

  (new-data-frame  (let ([cols : Columns (for/list ([col (data-frame-explode data-frame)])
		       (if (eq? name (column-heading col))
			   new-col
			   col))]) cols)))

; ***********************************************************

; ***********************************************************

; This functions consumes a DataFrame and either a Column,
; Listof Column or a DataFrame and extends the given DataFrame.
; If a DataFrame is passed for the second argument it is first
; exploded and appended on to the exploded given data frame. Then
; a new DataFrame is constructed and returned. The other cases
; are self-explanatory.
(: data-frame-extend (DataFrame (U Column Columns DataFrame) -> DataFrame))
(define (data-frame-extend data-frame cols)
  (cond 
   ((DataFrame? cols)
    (new-data-frame (append (data-frame-explode data-frame) (data-frame-explode cols))))
   ((list? cols)
    (new-data-frame (append (data-frame-explode data-frame) cols)))
   (else
    (new-data-frame (append (data-frame-explode data-frame) (list cols))))))

; ***********************************************************

; ***********************************************************
(: data-frame-set-index (DataFrame (U (Listof Label) (Listof Fixnum) (Listof Flonum) (Listof Datetime) RFIndex) -> DataFrame))
(define (data-frame-set-index data-frame new-index)
  (define src-series (DataFrame-series data-frame))
  (define src-column-names (map column-heading (data-frame-explode data-frame)))

  (: new-RFIndex RFIndex)
  (define new-RFIndex (LabelIndex (hash)))

  ; convert new-index to SIndex
  (if (list? new-index)
    (set! new-RFIndex (build-index-from-list (assert new-index list?)))
    (set! new-RFIndex new-index))

  (: new-columns Columns)
  (define new-columns
    (for/list ([pos (in-range (vector-length src-series))])
      ; define new column
      (cons (list-ref src-column-names pos) (set-series-index (vector-ref src-series pos) new-RFIndex))))

  (new-data-frame new-columns))
; ***********************************************************

; ***********************************************************
; Indexing into data-frame

(: data-frame-loc (DataFrame (U Label (Listof Label) (Listof Boolean)) LabelProjection -> (U Series DataFrame)))
(define (data-frame-loc data-frame label projection)
  ; This function consumes a DataFrame and LabelProjection and
  ; projects those columns.
  (: data-frame-cols (DataFrame LabelProjection -> Columns))
  (define (data-frame-cols data-frame project)
    (data-frame-explode data-frame #:project project))
  
  (define cols (data-frame-cols data-frame projection))

  (if (list? label)  
      (new-data-frame
       (for/list: : Columns ([col cols])
         (column (column-heading col) (assert (series-loc (column-series col) label) Series?))))
      (new-GenSeries
       (for/vector: : (Vectorof GenericType) ([col cols])
         (series-loc (column-series col) label)) #f)))

; doesn't preserve index currently, just gives new Range index

(: data-frame-iloc (DataFrame (U Index (Listof Index)) (U Index (Listof Index)) -> (U Series DataFrame)))
(define (data-frame-iloc data-frame idx-row idx-col)
  (if (and (list? idx-row) (list? idx-col))
      (data-frame-iloc-lsts data-frame idx-row idx-col)
      (data-frame-iloc-singles data-frame idx-row idx-col)))

; iloc works based on integer positioning. So no matter what your row labels are, you can always, e.g., get the first row by doing
; df.iloc[0, 0], takes in row and col indicies
(: data-frame-iloc-singles (DataFrame (U Index (Listof Index)) (U Index (Listof Index)) -> Series))
(define (data-frame-iloc-singles data-frame idx-row idx-col)
  ; This function consumes a DataFrame and LabelProjection and
  ; projects those columns.
  (: data-frame-cols (DataFrame LabelProjection -> Columns))
  (define (data-frame-cols data-frame project)
    (data-frame-explode data-frame #:project project))

  (let* ((project : LabelProjection
          (if (list? idx-col)
              (assert (map (lambda ([idx : Index]) (idx->key data-frame idx)) idx-col) LabelProjection?)
              (assert (list (idx->key data-frame idx-col)) LabelProjection?)))
         (cols (data-frame-cols data-frame project)))

     (if (list? idx-row)
         ; there will be only one column in this case, 2 lists can't be
         ; passed into this function
         (assert (series-iloc (column-series (car cols)) idx-row) Series?)
         (new-GenSeries
          (for/vector: : (Vectorof GenericType) ([col cols])
           (series-iloc (column-series col) idx-row)) #f))))

; iloc works based on integer positioning. So no matter what your row labels are, you can always, e.g., get the first row by doing
; df.iloc[0, 0], takes in row and col indicies
(: data-frame-iloc-lsts (DataFrame (Listof Index) (Listof Index) -> DataFrame))
(define (data-frame-iloc-lsts data-frame idx-row idx-col)
  ; This function consumes a DataFrame and LabelProjection and
  ; projects those columns.
  (: data-frame-cols (DataFrame LabelProjection -> Columns))
  (define (data-frame-cols data-frame project)
    (data-frame-explode data-frame #:project project))

  (: project LabelProjection)
  (define project (map (lambda ([idx : Index]) (idx->label data-frame idx)) idx-col))

  (define cols (data-frame-cols data-frame project))

  (new-data-frame
       (for/list: : Columns ([col cols])
         (cons (column-heading col) (assert (series-iloc (column-series col) idx-row) Series?)))))

(: data-frame-iloc-label (DataFrame (U Index (Listof Index)) LabelProjection -> (U Series DataFrame))) 
(define (data-frame-iloc-label data-frame idx projection)
  ; This function consumes a DataFrame and LabelProjection and
  ; projects those columns.
  (: data-frame-cols (DataFrame LabelProjection -> Columns))
  (define (data-frame-cols data-frame project)
    (data-frame-explode data-frame #:project project))
  
  (define cols (data-frame-cols data-frame projection))

  (if (list? idx)  
      (new-data-frame
       (for/list: : Columns ([col cols])
         (column (column-heading col) (assert (series-iloc (column-series col) idx) Series?))))
      (new-GenSeries
       (for/vector: : (Vectorof GenericType) ([col cols])
         (series-iloc (column-series col) idx)) #f)))

; ***********************************************************

(show-data-frame-description (data-frame-description (new-data-frame (hash 'a (list 1 2 3) 'b (list 3 5 6)))))