;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*    Author: Shubham Kahal
;*    File: data-frame-concat.rkt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket

; ***********************************************************
; data-frame-concat
; ***********************************************************

; ***********************************************************
; Provide functions in this file to other files.
(provide:
 [data-frame-concat-vertical (DataFrame DataFrame [#:col (Listof Symbol)] -> DataFrame)]
 [data-frame-concat-vertical-list ((Listof DataFrame) -> DataFrame)]
 [data-frame-concat-horizontal (DataFrame DataFrame -> DataFrame)]
 [data-frame-concat-horizontal-list ((Listof DataFrame) -> DataFrame)])

(require
 racket/pretty
 racket/unsafe/ops
 racket/flonum
 (only-in racket/set
	  set set-member?
	  list->set set->list
	  set-intersect set-subtract)
 (only-in "../util/symbol.rkt"
	  symbol-prefix)
 (only-in "indexed-series.rkt"
	  Label Labeling LabelProjection)
 (only-in "series.rkt"
	  series-complete series-data)
 (only-in "series-description.rkt"
	  SeriesType Series
	  SeriesDescription-type
	  series-type series-length)
 (only-in "data-frame.rkt"
	  DataFrame Columns new-data-frame data-frame-names data-frame-series-ref
	  data-frame-series data-frame-cseries data-frame-nseries data-frame-iseries data-frame-explode
	  DataFrameDescription DataFrameDescription-series data-frame-description column-series)
 (only-in "data-frame-join.rkt"
          dest-mapping-series-builders copy-column-row-error copy-column-row join-column-name)
 (only-in "numeric-series.rkt"
	  NSeries NSeries? nseries-iref new-NSeries)
 (only-in "generic-series.rkt"
          GenSeries GenSeries? gen-series-iref new-GenSeries
          gen-series-referencer)
 (only-in "integer-series.rkt"
	  ISeries ISeries? iseries-iref new-ISeries
	  iseries-referencer)
 (only-in "boolean-series.rkt"
	  BSeries BSeries? bseries-iref new-BSeries
	  bseries-referencer)
 (only-in "categorical-series.rkt"
	  cseries-referencer cseries-length cseries-iref
	  CSeries CSeries? new-CSeries)
 (only-in "series-builder.rkt"
	  SeriesBuilder)
 (only-in "generic-series-builder.rkt"
	  GenSeriesBuilder GenSeriesBuilder?
	  append-GenSeriesBuilder complete-GenSeriesBuilder
	  new-GenSeriesBuilder)
 (only-in "integer-series-builder.rkt"
	  ISeriesBuilder ISeriesBuilder?
	  append-ISeriesBuilder complete-ISeriesBuilder
	  new-ISeriesBuilder)
 (only-in "categorical-series-builder.rkt"
	  CSeriesBuilder CSeriesBuilder?
	  append-CSeriesBuilder complete-CSeriesBuilder
	  new-CSeriesBuilder)
 (only-in "categorical-series-ops.rkt"
	  cseries-append)
 (only-in "numeric-series-ops.rkt"
	  nseries-append)
 (only-in "integer-series-ops.rkt"
	  iseries-append)
 (only-in "numeric-series-builder.rkt"
	  NSeriesBuilder NSeriesBuilder?
	  append-NSeriesBuilder complete-NSeriesBuilder
	  new-NSeriesBuilder)
 (only-in "boolean-series-builder.rkt"
	  BSeriesBuilder BSeriesBuilder?
	  append-BSeriesBuilder complete-BSeriesBuilder
	  new-BSeriesBuilder))

; This functions consumes a Vectorof Series and Vectorof SeriesBuilder
; and an Index and does not return any value. It copies an entire row
; from the given Vectorof Series into the given Vectorof SeriesBuilders.
(: copy-null-to-row ((Vectorof Series) (Vectorof SeriesBuilder) -> Void))
(define (copy-null-to-row src-series dest-builders)
;;  (when (zero? (modulo row-id 10000))
;;	(displayln (format "Copy row: ~a" row-id)))
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
           ((ISeries? series)
            (if (ISeriesBuilder? builder)
                (append-ISeriesBuilder builder 0)
                (copy-column-row-error series col)))
           ((NSeries? series)
            (if (NSeriesBuilder? builder)
               (append-NSeriesBuilder builder +nan.0)
               (copy-column-row-error series col)))
           ((BSeries? series)
            (if (BSeriesBuilder? builder)
                (append-BSeriesBuilder builder #f)
                (copy-column-row-error series col)))))))


; This function consumes two DataFrames and produces a
; concatenation of all their series on matching column
; names.
(: data-frame-concat-vertical (DataFrame DataFrame [#:col (Listof Symbol)] -> DataFrame))
(define (data-frame-concat-vertical dfa dfb #:col [cols '()])

  (define: cols-a    : (Setof Label) (list->set (data-frame-names dfa)))
  (define: cols-b    : (Setof Label) (list->set (data-frame-names dfb)))
  (define: append-cols : (Setof Label) (if (null? cols)
					   (set-intersect cols-a cols-b)
                                           (set-intersect (list->set cols)
                                                          (set-intersect cols-a cols-b))))
  (new-data-frame (map (λ: ((name : Label))
                         (cons name
                               (let ((dfa-series (data-frame-series-ref dfa name))
                                     (dfb-series (data-frame-series-ref dfb name)))
                                 (if (not (equal? (series-type dfa-series) (series-type dfb-series)))
                                     (error 'data-frame-concat-vertical
                                            "The series types are different, unable to concat.") 
                                     (case (series-type dfa-series)
                                       ('CategoricalSeries (cseries-append (data-frame-cseries dfa name)
                                                                            (data-frame-cseries dfb name)))
                                       ('NumericSeries     (nseries-append (data-frame-nseries dfa name)
                                                                            (data-frame-nseries dfb name)))
                                       ('IntegerSeries     (iseries-append (data-frame-iseries dfa name)
                                                                            (data-frame-iseries dfb name)))
                                       (else (error 'data-frame-concat-vertical
                                                    "Unknown series type ~a."
                                                    (series-type dfa-series))))))))
                         (filter (λ: ((name : Label))
                                   (set-member? append-cols name))
                                 (data-frame-names dfa)))))

(: data-frame-concat-vertical-list ((Listof DataFrame) -> DataFrame))
(define (data-frame-concat-vertical-list data-frame-lst)
  (foldl (lambda ([data-frame-a : DataFrame] [data-frame-b : DataFrame])
           (data-frame-concat-vertical data-frame-a data-frame-b)) (car data-frame-lst) (cdr data-frame-lst)))

(: data-frame-concat-horizontal (DataFrame DataFrame -> DataFrame))
(define (data-frame-concat-horizontal dfa dfb)

  (define: cols-a    : (Setof Label) (list->set (data-frame-names dfa)))
  (define: cols-b    : (Setof Label) (list->set (data-frame-names dfb)))
  (define: dfa-cols    : Columns (data-frame-explode dfa))
  (define: dfb-cols    : Columns (data-frame-explode dfb))
  (define: dfa-len   : Fixnum (series-length (cdr (list-ref dfa-cols 0))))
  (define: dfb-len   : Fixnum (series-length (cdr (list-ref dfb-cols 0))))

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

  (cond
    [(> dfa-len dfb-len)
     ; Get series builders of default length 10 for all columns in fb.
     (define: dest-builders-b : (Vectorof SeriesBuilder)
       (list->vector (dest-mapping-series-builders (data-frame-description dfb) 10)))

     (for ([i (in-range dfb-len)])
       (copy-column-row (src-series (data-frame-cols dfb '())) dest-builders-b (assert i index?)))

     (for ([i (in-range (- dfa-len dfb-len))])
       (copy-null-to-row (src-series (data-frame-cols dfb '())) dest-builders-b))     

     (define: new-b-series : Columns
       (for/list ([builder (in-vector dest-builders-b)]
                  [col     (in-list dfb-cols)])
         (cons (join-column-name col cols-b "dfb-")
               (series-complete builder))))

     (new-data-frame (append dfa-cols new-b-series))]
    [(< dfa-len dfb-len)
     (define: dest-builders-a : (Vectorof SeriesBuilder)
       (list->vector (dest-mapping-series-builders (data-frame-description dfa) 10)))

     (for ([i (in-range dfa-len)])
       (copy-column-row (src-series (data-frame-cols dfa '())) dest-builders-a (assert i index?)))

     (for ([i (in-range (- dfb-len dfa-len))])
       (copy-null-to-row (src-series (data-frame-cols dfa '())) dest-builders-a))

     (define: new-a-series : Columns
       (for/list ([builder (in-vector dest-builders-a)]
                  [col     (in-list dfa-cols)])
         (cons (join-column-name col cols-a "dfa-")
            (series-complete builder))))

     (new-data-frame (append new-a-series dfb-cols))]
    [else
     (define: new-a-series : Columns
       (for/list ([col     (in-list dfa-cols)])
         (cons (join-column-name col cols-a "dfa-")
               (cdr col))))
     (new-data-frame (append new-a-series dfb-cols))])
  )

(: data-frame-concat-horizontal-list ((Listof DataFrame) -> DataFrame))
(define (data-frame-concat-horizontal-list data-frame-lst)
  (foldl data-frame-concat-horizontal (car data-frame-lst) (cdr data-frame-lst))) 
