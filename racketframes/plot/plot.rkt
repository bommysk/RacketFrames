#lang typed/racket

(require plot)
(require plot/utils)

(require
 racket/pretty
 racket/unsafe/ops
 racket/flonum
 racket/fixnum
 racket/set
 racket/vector
 (only-in racket/set
	  set set-member?
	  list->set set->list
	  set-intersect set-subtract)
 (only-in "../util/symbol.rkt"
	  symbol-prefix)
 (only-in "../data-frame/indexed-series.rkt"
	  IndexDataType build-index-from-list
	  Label SIndex LabelIndex LabelIndex-index
          label-index label->lst-idx
          idx->key is-labeled?)
 (only-in "../data-frame/series.rkt"
	  series-complete series-iref  
          series-data get-series-index has-series-index? SeriesDataVectorType)
 (only-in "../data-frame/series-description.rkt"
	  SeriesType Series Series? SeriesList SeriesList?
	  SeriesDescription-type series-type series-length)
 (only-in "../data-frame/data-frame.rkt"
	  DataFrame DataFrame? Column Columns Columns? Column? new-data-frame data-frame-names
	  data-frame-cseries data-frame-explode column-series
	  DataFrameDescription DataFrameDescription-series data-frame-description)
 (only-in "../data-frame/generic-series.rkt"
	  GenSeries GenSeries? GenericType gen-series-iref new-GenSeries
	  gen-series-referencer)
 (only-in "../data-frame/numeric-series.rkt"
	  NSeries NSeries? nseries-iref nseries-index-ref new-NSeries)
 (only-in "../data-frame/integer-series.rkt"
	  ISeries ISeries? iseries-iref new-ISeries
	  iseries-referencer)
 (only-in "../data-frame/categorical-series.rkt"
	  cseries-referencer cseries-length cseries-iref
	  CSeries CSeries? new-CSeries)
 (only-in "../data-frame/datetime-series.rkt"
	  datetime-series-referencer datetime-series-length datetime-series-iref
	  DatetimeSeries DatetimeSeries? new-DatetimeSeries)
 (only-in "../data-frame/date-series.rkt"
	  date-series-referencer date-series-length date-series-iref
	  DateSeries DateSeries? new-DateSeries)
 (only-in "../util/datetime/types.rkt"
          Datetime))

(provide
 PlottableSeries
 HistBin
 HistBinStacked)

(provide:
 [is-plottable-series (Series -> Boolean)]
 [get-series-point-sequence (Series -> (Listof (Listof Real)))]
 [get-column-point-sequence (Column -> (Listof (Listof Real)))]
 [get-series-to-series-point-sequence (Series Series -> (Listof (Listof Real)))]
 [get-data-frame-point-sequence (DataFrame -> (Listof (Listof (Listof Real))))]
 [get-series-list-point-sequence (SeriesList -> (Listof (Listof (Listof Real))))] 
 [get-columns-point-sequence (Columns -> (Listof (Listof (Listof Real))))]
 [make-points ((Listof (Listof Real)) -> renderer2d)]
 [make-lines ((Listof (Listof Real)) -> renderer2d)]
 [make-scatter-plot ((U Series (Listof Series) DataFrame Column Columns) [#:x-min Real] [#:x-max Real] [#:y-min Real] [#:y-max Real] -> Any)]
 [make-line-plot ((U Series (Listof Series) DataFrame Column Columns) [#:x-min Real] [#:x-max Real] [#:y-min Real] [#:y-max Real] -> Any)]
 [make-hist-bin-stacked (-> HistBinStacked)]
 [get-discrete-hist-data (PlotVector -> HistBin)] 
 [get-discrete-hist-data-vec (PlotVector PlottableSeries -> HistBinStacked)]
 [discrete-histogram-stacked-from-vector (PlotVector PlottableSeries -> (Listof renderer2d))]
 [make-discrete-histogram ((U Series (Listof Series) DataFrame Column Columns) -> Any)]
 [make-discrete-histogram-stacked ((U Series (Listof Series) DataFrame Column Columns) -> Any)]
 [get-index-vector (Series -> PlotVector)])

#|
#:x-min [x-min -10]
 	 	#:x-max [x-max 10]
 	 	#:y-min [y-min 10]
 	 	#:y-max y-max	 	 	 	 
 	 	#:sym sym	 	 	 	 
 	 	#:color color	 	 	 	 
 	 	#:fill-color fill-color	 	 	 	 
 	 	#:x-jitter x-jitter	 	 	 	 
 	 	#:y-jitter y-jitter	 	 	 	 
 	 	#:size size	 	 	 	 
 	 	#:line-width line-width	 	 	 	 
 	 	#:alpha alpha	 	 	 	 
 	 	#:label label |#

; ***********************************************************

(: is-plottable-series (Series -> Boolean))
(define (is-plottable-series series)
  (or (GenSeries? series) (ISeries? series) (NSeries? series) (DatetimeSeries? series) (DateSeries? series)))

(define-type PlottableSeries (U GenSeries NSeries ISeries DatetimeSeries DateSeries))

(define-predicate PlottableSeries? (U GenSeries NSeries ISeries DatetimeSeries DateSeries))

(define-type PlotVector (U SeriesDataVectorType (Vectorof IndexDataType) (Vectorof Fixnum)))

(define-predicate RealList? (Listof Real))

(define-predicate NestedRenderer2dList? (Listof (Listof renderer2d)))

(: get-series-point-sequence (Series -> (Listof (Listof Real))))
(define (get-series-point-sequence series)

  (when (or (eq? (series-type series) 'CategoricalSeries) (eq? (series-type series) 'BooleanSeries))
    (error 'get-series-point-sequence "Invalid series to plot."))
  
  (for/list: : (Listof (Listof Real))
    ([idx : Real (range (series-length series))])
    (list (add1 idx)
          (assert (car (assert (series-iref series (assert idx index?)) list?)) real?))))

(: get-column-point-sequence (Column -> (Listof (Listof Real))))
(define (get-column-point-sequence column)
  (get-series-point-sequence (column-series column)))

(: get-series-to-series-point-sequence (Series Series -> (Listof (Listof Real))))
(define (get-series-to-series-point-sequence series-1 series-2)

  (when (or
         (or (eq? (series-type series-1) 'CategoricalSeries) (eq? (series-type series-1) 'BooleanSeries))
         (or (eq? (series-type series-2) 'CategoricalSeries) (eq? (series-type series-2) 'BooleanSeries)))
    (error 'get-series-point-sequence "Invalid series to plot."))
  
  (for/list: : (Listof (Listof Real))
    ([idx : Real (range (series-length series-1))])
    (list (assert (car (assert (series-iref series-1 (assert idx index?)) list?)) real?)
          (assert (car (assert (series-iref series-2 (assert idx index?)) list?)) real?))))

(: get-data-frame-point-sequence (DataFrame -> (Listof (Listof (Listof Real)))))
(define (get-data-frame-point-sequence data-frame)
  (let* ((data-frame-exploded (data-frame-explode data-frame))
         (x-series (cdr (car data-frame-exploded)))
         (y-series (map column-series (cdr data-frame-exploded))))
    (map (lambda ([y : Series]) (get-series-to-series-point-sequence x-series y)) y-series)))

(: get-series-list-point-sequence (SeriesList -> (Listof (Listof (Listof Real)))))
(define (get-series-list-point-sequence series-list)
  (let ((x-series (car series-list))
        (y-series (cdr series-list)))
    (map (lambda ([y : Series]) (get-series-to-series-point-sequence x-series y)) y-series)))

(: get-columns-point-sequence (Columns -> (Listof (Listof (Listof Real)))))
(define (get-columns-point-sequence columns)
  (let ((x-series (column-series (car columns)))
        (y-series (map column-series (cdr columns))))
    (map (lambda ([y : Series]) (get-series-to-series-point-sequence x-series y)) y-series)))

(: make-points ((Listof (Listof Real)) -> renderer2d))
(define (make-points list-of-points)
  (points list-of-points))

(: make-lines ((Listof (Listof Real)) -> renderer2d))
(define (make-lines list-of-points)
  (lines list-of-points))

(: make-scatter-plot ((U Series (Listof Series) DataFrame Column Columns) [#:x-min Real] [#:x-max Real] [#:y-min Real] [#:y-max Real]  -> Any))
(define (make-scatter-plot data #:x-min [x-min -20] #:x-max [x-max 20] #:y-min [y-min -20] #:y-max [y-max 20])
  (let: ((plot-points : (Treeof renderer2d)
         (cond
           [(and (Series? data) (is-plottable-series data)) (list (points (get-series-point-sequence data)))]
           [(and (SeriesList? data) (andmap is-plottable-series data)) (map make-points (get-series-list-point-sequence data))]
           [(Column? data) (list (points (get-column-point-sequence data)))]
           [(Columns? data) (map make-points (get-columns-point-sequence data))]
           [(DataFrame? data) (map make-points (get-data-frame-point-sequence data))]
           [else (error 'make-scatter-plot "Invalid data to plot")])))
    (plot
     plot-points         
     #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max)))

(: make-line-plot ((U Series (Listof Series) DataFrame Column Columns) [#:x-min Real] [#:x-max Real] [#:y-min Real] [#:y-max Real] -> Any))
(define (make-line-plot data #:x-min [x-min -20] #:x-max [x-max 20] #:y-min [y-min -20] #:y-max [y-max 20])
  (let: ((plot-points : (Treeof renderer2d)
         (cond
           [(and (Series? data) (is-plottable-series data)) (list (lines (get-series-point-sequence data)))]
           [(and (SeriesList? data) (andmap is-plottable-series data)) (map make-lines (get-series-list-point-sequence data))]
           [(Column? data) (list (lines (get-column-point-sequence data)))]
           [(Columns? data) (map make-lines (get-columns-point-sequence data))]
           [(DataFrame? data) (map make-lines (get-data-frame-point-sequence data))]
           [else (error 'make-line-plot "Invalid data to plot")])))
    (plot
     plot-points         
     #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max)))

; ***********************************************************

; ***********************************************************

(define-type HistBin (HashTable Any Real))

(define-type HistBinStacked (HashTable Any (Listof Real)))

; This function is self-explanatory, it consumes no arguments
; and creates a hash map which will represent a JoinHash.
(: make-hist-bin (-> HistBin))
(define (make-hist-bin)
  (make-hash))

; This function is self-explanatory, it consumes no arguments
; and creates a hash map which will represent a JoinHash.
(: make-hist-bin-stacked (-> HistBinStacked))
(define (make-hist-bin-stacked)
  (make-hash))

(: get-flvector/vector-length ((U FlVector (Vectorof Fixnum)) -> Index))
(define (get-flvector/vector-length flvector/fxvector)
  (if (flvector? flvector/fxvector)
        (flvector-length flvector/fxvector)
        (vector-length flvector/fxvector)))

(: get-flvector/vector-ref ((U FlVector (Vectorof Fixnum)) Index -> (U Float Fixnum)))
(define (get-flvector/vector-ref flvector/fxvector i)
  (if (flvector? flvector/fxvector)
        (flvector-ref flvector/fxvector i)
        (vector-ref flvector/fxvector i)))
  

; Used to determine the groups for the groupby. If by is a function, it’s called
; on each value of the object’s index. If a dict or Series is passed, the Series
; or dict VALUES will be used to determine the groups (the Series’ values are first
; aligned; see .align() method). If an ndarray is passed, the values are used as-is
; determine the groups. A label or list of labels may be passed to group by the columns
; in self. Notice that a tuple is interpreted a (single) key.
(: get-discrete-hist-data (PlotVector -> HistBin))
(define (get-discrete-hist-data data-vec)
  (define: hist-bin-index : HistBin (make-hist-bin))
  (define len
    (if (flvector? data-vec)
        (flvector-length data-vec)
        (vector-length data-vec)))

  (let loop ([i 0])
    (if (>= i len)
	hist-bin-index
	(let: ((i : Index (assert i index?)))
	      (let ((key

                     (if (flvector? data-vec)
                         (flvector-ref data-vec i)
                         (vector-ref data-vec i))))
		(hash-update! hist-bin-index key
			      (λ: ((incr : Real))
				  (add1 incr))
			      (λ () 0)))
	      (loop (add1 i))))))

(: get-discrete-hist-data-vec (PlotVector PlottableSeries -> HistBinStacked))
(define (get-discrete-hist-data-vec data-vec-one data-series-two)
  (define: hist-bin-index : HistBinStacked (make-hist-bin-stacked))
  (define vec-one-len
    (if (flvector? data-vec-one)
        (flvector-length data-vec-one)
        (vector-length data-vec-one)))

  (define series-two-len
    (series-length data-series-two))

  (when (not (= vec-one-len series-two-len))
    (error "Vectors must be of the same length."))

  (let loop ([i 0])
    (if (>= i vec-one-len)
	hist-bin-index
	(let: ((i : Index (assert i index?)))
	      (let ((key
                     (if (flvector? data-vec-one)
                         (flvector-ref data-vec-one i)
                         (vector-ref data-vec-one i))))
		(hash-update! hist-bin-index key
			      (λ: ((val-list : (Listof Real)))
				  (append val-list (assert (series-iref data-series-two i) RealList?)))
			      (λ () (list)))
          (loop (add1 i)))))))

(: list-of-vec-from-hist-bin (HistBin -> (Listof (Vector Any Real))))
(define (list-of-vec-from-hist-bin hist-bin)
  (hash-map hist-bin (lambda ([key : Any] [value : Real]) : (Vector Any Real) (vector key value))))

(: list-of-vec-from-hist-bin-stacked (HistBinStacked -> (Listof (Vector Any (Listof Real)))))
(define (list-of-vec-from-hist-bin-stacked hist-bin-stacked)
  (hash-map hist-bin-stacked (lambda ([key : Any] [value : (Listof Real)]) : (Vector Any (Listof Real)) (vector key value))))

(: discrete-histogram-from-vector (PlotVector -> renderer2d))
(define (discrete-histogram-from-vector vec)
  (discrete-histogram (cast (list-of-vec-from-hist-bin (get-discrete-hist-data vec))
                            (Sequenceof (U (List Any (U False Real ivl)) (Vector Any (U False Real ivl)))))))

(: discrete-histogram-stacked-from-vector (PlotVector PlottableSeries -> (Listof renderer2d)))
(define (discrete-histogram-stacked-from-vector data-vec-one data-series-two)
  (stacked-histogram (cast (list-of-vec-from-hist-bin-stacked (get-discrete-hist-data-vec data-vec-one (assert data-series-two PlottableSeries?))) 	
                            (Sequenceof (U (Vector Any (Sequenceof Real)) (List Any (Sequenceof Real)))))))

;ann
(: make-discrete-histogram ((U Series (Listof Series) DataFrame Column Columns) -> Any))
(define (make-discrete-histogram data)
  (let: ((plot-points : (U renderer2d (Listof renderer2d))
         (cond
           [(and (Series? data) (is-plottable-series data))
            (discrete-histogram-from-vector (series-data data))]
           [(and (SeriesList? data) (andmap is-plottable-series data))
            (map (lambda ([d : Series]) : renderer2d
                  (discrete-histogram-from-vector (series-data d))) data)]
           [(and (Column? data) (is-plottable-series (column-series data)))
            (discrete-histogram-from-vector (series-data (column-series data)))]
           [(and (Columns? data) (andmap is-plottable-series (map (lambda ([d : Column]) (column-series d)) data)))
            (map (lambda ([d : Column])
             (discrete-histogram-from-vector (series-data (column-series d)))) data)]
           ; A histogram is a representation of the distribution of data. This function calls matplotlib.pyplot.hist(),
           ;on each series in the DataFrame, resulting in one histogram per column.
           [(and (DataFrame? data) (andmap is-plottable-series (map (lambda ([d : Column]) (column-series d)) (data-frame-explode data))))
            (map (lambda ([d : Column])
             (discrete-histogram-from-vector (series-data (column-series d)))) (data-frame-explode data))]
           [else (error 'make-scatter-plot "Invalid data to plot")])))
    (if (list? plot-points)
        (map (lambda ([p-p : renderer2d]) (plot p-p)) plot-points)
        (plot
         plot-points))))

(: get-index-vector (Series -> PlotVector))
(define (get-index-vector series)
  (: series-len Index)
  (define series-len (series-length series))
  (define v (series-data series))
  ; initialize vectors with default values
  (: index-data-type-vector (Vectorof IndexDataType))
  (define index-data-type-vector (make-vector (assert series-len index?) 0))
  (: index-vector (Vectorof Fixnum))
  (define index-vector (make-vector (assert series-len index?) 0))
  
  (let ((len series-len))
    (if (zero? len)
        (vector)
        (begin
          (do ((i 0 (add1 i)))
            ((>= i len)
             (if (has-series-index? series)                  
                  index-data-type-vector
                  index-vector))
            (if (has-series-index? series)
                (vector-set! index-data-type-vector (assert i index?) (idx->key (get-series-index series) (assert i index?)))
                (vector-set! index-vector (assert i index?) (assert i index?))))))))

; In the case of multiple data vectors, the first vector is plotted against all other vectors. Else in the singular case,
; the index is plotted against the vector. The DataFrame can be projected to only use a subset of columns.
(: make-discrete-histogram-stacked ((U Series (Listof Series) DataFrame Column Columns) -> Any))
(define (make-discrete-histogram-stacked data)
  (let: ((plot-points : (U (Listof (Listof renderer2d)) (Listof renderer2d))
         (cond
           [(and (Series? data) (is-plottable-series data))
            (discrete-histogram-stacked-from-vector (get-index-vector data) (assert data PlottableSeries?))]
           [(and (SeriesList? data) (andmap is-plottable-series data))
            (map (lambda ([d : Series]) : (Listof renderer2d)
                   (discrete-histogram-stacked-from-vector (series-data (car data)) (assert d PlottableSeries?))) (cdr data))]
           [(and (Column? data) (is-plottable-series (column-series data)))
            (discrete-histogram-stacked-from-vector (get-index-vector (column-series data)) (assert (column-series data) PlottableSeries?))]
           [(and (Columns? data) (andmap is-plottable-series (map (lambda ([d : Column]) (column-series d)) data)))
            (map (lambda ([d : Column])
                   (discrete-histogram-stacked-from-vector (series-data (column-series (car data))) (assert (column-series d) PlottableSeries?))) (cdr data))]
           ; A histogram is a representation of the distribution of data. This function calls matplotlib.pyplot.hist(),
           ;on each series in the DataFrame, resulting in one histogram per column.
           [(and (DataFrame? data) (andmap is-plottable-series (map (lambda ([d : Column]) (column-series d)) (data-frame-explode data))))
            (let ((columns : Columns (data-frame-explode data)))
              (map (lambda ([d : Column])
                     (discrete-histogram-stacked-from-vector (series-data (column-series (car columns))) (assert (column-series d) PlottableSeries?))) (cdr columns)))]
           [else (error 'make-scatter-plot "Invalid data to plot")])))
    
        (if (NestedRenderer2dList? plot-points)
            (map (lambda ([p-p : (Listof renderer2d)]) (plot p-p)) plot-points)
            (plot
             plot-points))))

; ***********************************************************
