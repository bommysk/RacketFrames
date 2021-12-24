#lang typed/racket

; ***********************************************************
; Provide functions in this file to other files.

(provide:
 [data-frame+ (DataFrame DataFrame -> DataFrame)]
 [data-frame- (DataFrame DataFrame -> DataFrame)]
 [data-frame* (DataFrame DataFrame -> DataFrame)]
 [data-frame/ (DataFrame DataFrame -> DataFrame)]
 [data-frame% (DataFrame DataFrame -> DataFrame)]
 [data-frame-r (DataFrame DataFrame -> DataFrame)]
 [data-frame= (DataFrame DataFrame -> DataFrame)]
 [data-frame!= (DataFrame DataFrame -> DataFrame)]
 [data-frame>= (DataFrame DataFrame -> DataFrame)]
 [data-frame<= (DataFrame DataFrame -> DataFrame)]
 [data-frame> (DataFrame DataFrame -> DataFrame)]
 [data-frame< (DataFrame DataFrame -> DataFrame)]
 [data-frame-abs (DataFrame -> DataFrame)]
 [data-frame-filter (DataFrame BSeries -> DataFrame)]
 [data-frame-column-filter (DataFrame (Any -> Boolean) Label -> DataFrame)]
 [data-frame-apply (DataFrame (Any -> Any) -> DataFrame)])
; ***********************************************************

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
	  Label Labeling LabelProjection RFIndex index-idxes)
 (only-in "series.rkt"
	  new-series get-series-index set-series-index series-complete series-data series-filter series-iloc
          series-data-idxes-from-predicate)
 (only-in "series-description.rkt"
	  SeriesType Series Series?
	  SeriesDescription-type
	  series-type series-length)
 (only-in "data-frame.rkt"
	  DataFrame Columns Column column new-data-frame data-frame-series data-frame-names column-heading
	  data-frame-series-ref data-frame-cseries data-frame-nseries data-frame-iseries data-frame-explode
	  DataFrameDescription DataFrameDescription-series data-frame-description column-series data-frame-index)
 (only-in "data-frame-join.rkt"
          dest-mapping-series-builders copy-column-row)
 (only-in "numeric-series.rkt"
	  NSeries NSeries? nseries-iref new-NSeries
          +/ns -/ns */ns //ns >/ns </ns
          >=/ns <=/ns =/ns !=/ns +./ns
          -./ns *./ns /./ns +/ns/is -/ns/is
          */ns/is //ns/is +/is/ns -/is/ns */is/ns
          //is/ns >/ns/is </ns/is >=/ns/is <=/ns/is 
          =/ns/is !=/ns/is >/is/ns </is/ns >=/is/ns 
          <=/is/ns =/is/ns !=/is/ns flvector->list)
 (only-in "numeric-series-ops.rkt"
          nseries-abs)
 (only-in "integer-series.rkt"
	  ISeries ISeries? iseries-iref new-ISeries
	  iseries-referencer +/is -/is */is //is
          %/is r/is >/is </is >=/is <=/is =/is !=/is)
 (only-in "integer-series-ops.rkt"
          iseries-abs)
 (only-in "boolean-series.rkt"
	  BSeries BSeries? bseries-iref new-BSeries
	  bseries-referencer bseries-data)
 (only-in "categorical-series.rkt"
	  cseries-referencer cseries-length cseries-iref
	  CSeries CSeries? new-CSeries)
 (only-in "series-builder.rkt"
	  SeriesBuilder)
 (only-in "integer-series-builder.rkt"
	  ISeriesBuilder ISeriesBuilder?
	  append-ISeriesBuilder complete-ISeriesBuilder
	  new-ISeriesBuilder)
 (only-in "categorical-series-builder.rkt"
	  CSeriesBuilder CSeriesBuilder?
	  append-CSeriesBuilder complete-CSeriesBuilder
	  new-CSeriesBuilder)
 (only-in "numeric-series-builder.rkt"
	  NSeriesBuilder NSeriesBuilder?
	  append-NSeriesBuilder complete-NSeriesBuilder
	  new-NSeriesBuilder)
 (only-in "boolean-series-builder.rkt"
	  BSeriesBuilder BSeriesBuilder?
	  append-BSeriesBuilder complete-BSeriesBuilder
	  new-BSeriesBuilder)
 (only-in "data-frame-print.rkt"
          data-frame-write-delim))


;(: data-frame+ (DataFrame DataFrame -> DataFrame))
(: data-frame+ (DataFrame DataFrame -> DataFrame))
(define (data-frame+ dfa dfb)
  (define: cols-a    : (Setof Label) (list->set (data-frame-names dfa)))
  (define: cols-b    : (Setof Label) (list->set (data-frame-names dfb)))
  (define: append-cols : (Setof Label) (set-intersect cols-a cols-b))
  
  (new-data-frame (map (λ: ((name : Label))
                         (cons name
                               (let ((dfa-series (data-frame-series-ref dfa name))
                                     (dfb-series (data-frame-series-ref dfb name))) 
                                 (match (list (series-type dfa-series) (series-type dfb-series))
                                   ((list 'NumericSeries 'IntegerSeries)     (+/ns/is (data-frame-nseries dfa name)
                                                                                      (data-frame-iseries dfb name)))
                                   ((list 'IntegerSeries 'NumericSeries)    (+/is/ns (data-frame-iseries dfa name)
                                                                                     (data-frame-nseries dfb name)))
                                   ((list 'IntegerSeries 'IntegerSeries)    (+/is (data-frame-iseries dfa name)
                                                                                  (data-frame-iseries dfb name)))
                                   ((list 'NumericSeries 'NumericSeries)    (+/ns (data-frame-nseries dfa name)
                                                                                  (data-frame-nseries dfb name)))
                                   (else (error 'data-frame+
                                                "Incorrect series type ~a ~a, must be Integer or Numeric."
                                                (series-type dfa-series) (series-type dfb-series)))))))
                         (filter (λ: ((name : Label))
                                   (set-member? append-cols name))
                                 (data-frame-names dfa)))))

;(: data-frame- (DataFrame DataFrame [#:on (Listof Symbol)] -> DataFrame))
(: data-frame- (DataFrame DataFrame -> DataFrame))
(define (data-frame- dfa dfb)
  (define: cols-a    : (Setof Label) (list->set (data-frame-names dfa)))
  (define: cols-b    : (Setof Label) (list->set (data-frame-names dfb)))
  (define: append-cols : (Setof Label) (set-intersect cols-a cols-b))
  
  (new-data-frame (map (λ: ((name : Label))
                         (cons name
                               (let ((dfa-series (data-frame-series-ref dfa name))
                                     (dfb-series (data-frame-series-ref dfb name))) 
                                 (match (list (series-type dfa-series) (series-type dfb-series))
                                   ((list 'NumericSeries 'IntegerSeries)     (-/ns/is (data-frame-nseries dfa name)
                                                                                      (data-frame-iseries dfb name)))
                                   ((list 'IntegerSeries 'NumericSeries)    (-/is/ns (data-frame-iseries dfa name)
                                                                                     (data-frame-nseries dfb name)))
                                   ((list 'IntegerSeries 'IntegerSeries)    (-/is (data-frame-iseries dfa name)
                                                                                  (data-frame-iseries dfb name)))
                                   ((list 'NumericSeries 'NumericSeries)    (-/ns (data-frame-nseries dfa name)
                                                                                  (data-frame-nseries dfb name)))
                                   (else (error 'data-frame-
                                                "Incorrect series type ~a ~a, must be Integer or Numeric."
                                                (series-type dfa-series) (series-type dfb-series)))))))
                         (filter (λ: ((name : Label))
                                   (set-member? append-cols name))
                                 (data-frame-names dfa)))))

;(: data-frame* (DataFrame DataFrame [#:on (Listof Symbol)] -> DataFrame))
(: data-frame* (DataFrame DataFrame -> DataFrame))
(define (data-frame* dfa dfb)
  (define: cols-a    : (Setof Label) (list->set (data-frame-names dfa)))
  (define: cols-b    : (Setof Label) (list->set (data-frame-names dfb)))
  (define: append-cols : (Setof Label) (set-intersect cols-a cols-b))
  
  (new-data-frame (map (λ: ((name : Label))
                         (cons name
                               (let ((dfa-series (data-frame-series-ref dfa name))
                                     (dfb-series (data-frame-series-ref dfb name))) 
                                 (match (list (series-type dfa-series) (series-type dfb-series))
                                   ((list 'NumericSeries 'IntegerSeries)     (*/ns/is (data-frame-nseries dfa name)
                                                                                      (data-frame-iseries dfb name)))
                                   ((list 'IntegerSeries 'NumericSeries)    (*/is/ns (data-frame-iseries dfa name)
                                                                                     (data-frame-nseries dfb name)))
                                   ((list 'IntegerSeries 'IntegerSeries)    (*/is (data-frame-iseries dfa name)
                                                                                  (data-frame-iseries dfb name)))
                                   ((list 'NumericSeries 'NumericSeries)    (*/ns (data-frame-nseries dfa name)
                                                                                  (data-frame-nseries dfb name)))
                                   (else (error 'data-frame*
                                                "Incorrect series type ~a ~a, must be Integer or Numeric."
                                                (series-type dfa-series) (series-type dfb-series)))))))
                         (filter (λ: ((name : Label))
                                   (set-member? append-cols name))
                                 (data-frame-names dfa)))))

;(: data-frame/ (DataFrame DataFrame [#:on (Listof Symbol)] -> DataFrame))
(: data-frame/ (DataFrame DataFrame -> DataFrame))
(define (data-frame/ dfa dfb)
  (define: cols-a    : (Setof Label) (list->set (data-frame-names dfa)))
  (define: cols-b    : (Setof Label) (list->set (data-frame-names dfb)))
  (define: append-cols : (Setof Label) (set-intersect cols-a cols-b))
  
  (new-data-frame (map (λ: ((name : Label))
                         (cons name
                               (let ((dfa-series (data-frame-series-ref dfa name))
                                     (dfb-series (data-frame-series-ref dfb name))) 
                                 (match (list (series-type dfa-series) (series-type dfb-series))
                                   ((list 'NumericSeries 'IntegerSeries)     (//ns/is (data-frame-nseries dfa name)
                                                                                      (data-frame-iseries dfb name)))
                                   ((list 'IntegerSeries 'NumericSeries)    (//is/ns (data-frame-iseries dfa name)
                                                                                     (data-frame-nseries dfb name)))
                                   ((list 'IntegerSeries 'IntegerSeries)    (//is (data-frame-iseries dfa name)
                                                                                  (data-frame-iseries dfb name)))
                                   ((list 'NumericSeries 'NumericSeries)    (//ns (data-frame-nseries dfa name)
                                                                                  (data-frame-nseries dfb name)))
                                   (else (error 'data-frame/
                                                "Incorrect series type ~a ~a, must be Integer or Numeric."
                                                (series-type dfa-series) (series-type dfb-series)))))))
                         (filter (λ: ((name : Label))
                                   (set-member? append-cols name))
                                 (data-frame-names dfa)))))

;(: data-frame% (DataFrame DataFrame [#:on (Listof Symbol)] -> DataFrame))
(: data-frame% (DataFrame DataFrame -> DataFrame))
(define (data-frame% dfa dfb)
  (define: cols-a    : (Setof Label) (list->set (data-frame-names dfa)))
  (define: cols-b    : (Setof Label) (list->set (data-frame-names dfb)))
  (define: append-cols : (Setof Label) (set-intersect cols-a cols-b))
  
  (new-data-frame (map (λ: ((name : Label))
                         (cons name
                               (let ((dfa-series (data-frame-series-ref dfa name))
                                     (dfb-series (data-frame-series-ref dfb name))) 
                                 (match (list (series-type dfa-series) (series-type dfb-series))                                                                      
                                   ((list 'IntegerSeries 'IntegerSeries)    (%/is (data-frame-iseries dfa name)
                                                                                  (data-frame-iseries dfb name)))                                   
                                   (else (error 'data-frame%
                                                "Incorrect series type ~a ~a, must be Integer or Numeric."
                                                (series-type dfa-series) (series-type dfb-series)))))))
                         (filter (λ: ((name : Label))
                                   (set-member? append-cols name))
                                 (data-frame-names dfa)))))

(: data-frame-r (DataFrame DataFrame -> DataFrame))
(define (data-frame-r dfa dfb)
  (define: cols-a    : (Setof Label) (list->set (data-frame-names dfa)))
  (define: cols-b    : (Setof Label) (list->set (data-frame-names dfb)))
  (define: append-cols : (Setof Label) (set-intersect cols-a cols-b))
  
  (new-data-frame (map (λ: ((name : Label))
                         (cons name
                               (let ((dfa-series (data-frame-series-ref dfa name))
                                     (dfb-series (data-frame-series-ref dfb name))) 
                                 (match (list (series-type dfa-series) (series-type dfb-series))                                                                      
                                   ((list 'IntegerSeries 'IntegerSeries)    (r/is (data-frame-iseries dfa name)
                                                                                  (data-frame-iseries dfb name)))                                   
                                   (else (error 'data-frame-r
                                                "Incorrect series type ~a ~a, must be Integer or Numeric."
                                                (series-type dfa-series) (series-type dfb-series)))))))
                         (filter (λ: ((name : Label))
                                   (set-member? append-cols name))
                                 (data-frame-names dfa)))))

;(: data-frame= (DataFrame DataFrame [#:on (Listof Symbol)] -> DataFrame))
(: data-frame= (DataFrame DataFrame -> DataFrame))
(define (data-frame= dfa dfb)
  (define: cols-a    : (Setof Label) (list->set (data-frame-names dfa)))
  (define: cols-b    : (Setof Label) (list->set (data-frame-names dfb)))
  (define: matching-cols : (Setof Label) (set-intersect cols-a cols-b))
  
  (new-data-frame (map (λ: ((name : Label))
                         (cons name
                               (let ((dfa-series (data-frame-series-ref dfa name))
                                     (dfb-series (data-frame-series-ref dfb name))) 
                                 (match (list (series-type dfa-series) (series-type dfb-series))
                                   ((list 'NumericSeries 'IntegerSeries)     (=/ns/is (data-frame-nseries dfa name)
                                                                                      (data-frame-iseries dfb name)))
                                   ((list 'IntegerSeries 'NumericSeries)    (=/is/ns (data-frame-iseries dfa name)
                                                                                     (data-frame-nseries dfb name)))
                                   ((list 'IntegerSeries 'IntegerSeries)    (=/is (data-frame-iseries dfa name)
                                                                                  (data-frame-iseries dfb name)))
                                   ((list 'NumericSeries 'NumericSeries)    (=/ns (data-frame-nseries dfa name)
                                                                                  (data-frame-nseries dfb name)))
                                   (else (error 'data-frame=
                                                "Incorrect series type ~a ~a, must be Integer or Numeric."
                                                (series-type dfa-series) (series-type dfb-series)))))))
                         (filter (λ: ((name : Label))
                                   (set-member? matching-cols name))
                                 (data-frame-names dfa)))))

;(: data-frame!= (DataFrame DataFrame [#:on (Listof Symbol)] -> DataFrame))
(: data-frame!= (DataFrame DataFrame -> DataFrame))
(define (data-frame!= dfa dfb)
  (define: cols-a    : (Setof Label) (list->set (data-frame-names dfa)))
  (define: cols-b    : (Setof Label) (list->set (data-frame-names dfb)))
  (define: matching-cols : (Setof Label) (set-intersect cols-a cols-b))
  
  (new-data-frame (map (λ: ((name : Label))
                         (cons name
                               (let ((dfa-series (data-frame-series-ref dfa name))
                                     (dfb-series (data-frame-series-ref dfb name))) 
                                 (match (list (series-type dfa-series) (series-type dfb-series))
                                   ((list 'NumericSeries 'IntegerSeries)     (!=/ns/is (data-frame-nseries dfa name)
                                                                                      (data-frame-iseries dfb name)))
                                   ((list 'IntegerSeries 'NumericSeries)    (!=/is/ns (data-frame-iseries dfa name)
                                                                                     (data-frame-nseries dfb name)))
                                   ((list 'IntegerSeries 'IntegerSeries)    (!=/is (data-frame-iseries dfa name)
                                                                                  (data-frame-iseries dfb name)))
                                   ((list 'NumericSeries 'NumericSeries)    (!=/ns (data-frame-nseries dfa name)
                                                                                  (data-frame-nseries dfb name)))
                                   (else (error 'data-frame!=
                                                "Incorrect series type ~a ~a, must be Integer or Numeric."
                                                (series-type dfa-series) (series-type dfb-series)))))))
                         (filter (λ: ((name : Label))
                                   (set-member? matching-cols name))
                                 (data-frame-names dfa)))))

;(: data-frame< (DataFrame DataFrame [#:on (Listof Symbol)] -> DataFrame))
(: data-frame< (DataFrame DataFrame -> DataFrame))
(define (data-frame< dfa dfb)
  (define: cols-a    : (Setof Label) (list->set (data-frame-names dfa)))
  (define: cols-b    : (Setof Label) (list->set (data-frame-names dfb)))
  (define: matching-cols : (Setof Label) (set-intersect cols-a cols-b))
  
  (new-data-frame (map (λ: ((name : Label))
                         (cons name
                               (let ((dfa-series (data-frame-series-ref dfa name))
                                     (dfb-series (data-frame-series-ref dfb name))) 
                                 (match (list (series-type dfa-series) (series-type dfb-series))
                                   ((list 'NumericSeries 'IntegerSeries)     (</ns/is (data-frame-nseries dfa name)
                                                                                      (data-frame-iseries dfb name)))
                                   ((list 'IntegerSeries 'NumericSeries)    (</is/ns (data-frame-iseries dfa name)
                                                                                     (data-frame-nseries dfb name)))
                                   ((list 'IntegerSeries 'IntegerSeries)    (</is (data-frame-iseries dfa name)
                                                                                  (data-frame-iseries dfb name)))
                                   ((list 'NumericSeries 'NumericSeries)    (</ns (data-frame-nseries dfa name)
                                                                                  (data-frame-nseries dfb name)))
                                   (else (error 'data-frame=
                                                "Incorrect series type ~a ~a, must be Integer or Numeric."
                                                (series-type dfa-series) (series-type dfb-series)))))))
                         (filter (λ: ((name : Label))
                                   (set-member? matching-cols name))
                                 (data-frame-names dfa)))))

;(: data-frame> (DataFrame DataFrame [#:on (Listof Symbol)] -> DataFrame))
(: data-frame> (DataFrame DataFrame -> DataFrame))
(define (data-frame> dfa dfb)
  (define: cols-a    : (Setof Label) (list->set (data-frame-names dfa)))
  (define: cols-b    : (Setof Label) (list->set (data-frame-names dfb)))
  (define: matching-cols : (Setof Label) (set-intersect cols-a cols-b))
  
  (new-data-frame (map (λ: ((name : Label))
                         (cons name
                               (let ((dfa-series (data-frame-series-ref dfa name))
                                     (dfb-series (data-frame-series-ref dfb name))) 
                                 (match (list (series-type dfa-series) (series-type dfb-series))
                                   ((list 'NumericSeries 'IntegerSeries)     (>/ns/is (data-frame-nseries dfa name)
                                                                                      (data-frame-iseries dfb name)))
                                   ((list 'IntegerSeries 'NumericSeries)    (>/is/ns (data-frame-iseries dfa name)
                                                                                     (data-frame-nseries dfb name)))
                                   ((list 'IntegerSeries 'IntegerSeries)    (>/is (data-frame-iseries dfa name)
                                                                                  (data-frame-iseries dfb name)))
                                   ((list 'NumericSeries 'NumericSeries)    (>/ns (data-frame-nseries dfa name)
                                                                                  (data-frame-nseries dfb name)))
                                   (else (error 'data-frame>
                                                "Incorrect series type ~a ~a, must be Integer or Numeric."
                                                (series-type dfa-series) (series-type dfb-series)))))))
                         (filter (λ: ((name : Label))
                                   (set-member? matching-cols name))
                                 (data-frame-names dfa)))))

;(: data-frame<= (DataFrame DataFrame [#:on (Listof Symbol)] -> DataFrame))
(: data-frame<= (DataFrame DataFrame -> DataFrame))
(define (data-frame<= dfa dfb)
  (define: cols-a    : (Setof Label) (list->set (data-frame-names dfa)))
  (define: cols-b    : (Setof Label) (list->set (data-frame-names dfb)))
  (define: matching-cols : (Setof Label) (set-intersect cols-a cols-b))
  
  (new-data-frame (map (λ: ((name : Label))
                         (cons name
                               (let ((dfa-series (data-frame-series-ref dfa name))
                                     (dfb-series (data-frame-series-ref dfb name))) 
                                 (match (list (series-type dfa-series) (series-type dfb-series))
                                   ((list 'NumericSeries 'IntegerSeries)     (<=/ns/is (data-frame-nseries dfa name)
                                                                                      (data-frame-iseries dfb name)))
                                   ((list 'IntegerSeries 'NumericSeries)    (<=/is/ns (data-frame-iseries dfa name)
                                                                                     (data-frame-nseries dfb name)))
                                   ((list 'IntegerSeries 'IntegerSeries)    (<=/is (data-frame-iseries dfa name)
                                                                                  (data-frame-iseries dfb name)))
                                   ((list 'NumericSeries 'NumericSeries)    (<=/ns (data-frame-nseries dfa name)
                                                                                  (data-frame-nseries dfb name)))
                                   (else (error 'data-frame<=
                                                "Incorrect series type ~a ~a, must be Integer or Numeric."
                                                (series-type dfa-series) (series-type dfb-series)))))))
                         (filter (λ: ((name : Label))
                                   (set-member? matching-cols name))
                                 (data-frame-names dfa)))))

;(: data-frame>= (DataFrame DataFrame [#:on (Listof Symbol)] -> DataFrame))
(: data-frame>= (DataFrame DataFrame -> DataFrame))
(define (data-frame>= dfa dfb)
  (define: cols-a    : (Setof Label) (list->set (data-frame-names dfa)))
  (define: cols-b    : (Setof Label) (list->set (data-frame-names dfb)))
  (define: matching-cols : (Setof Label) (set-intersect cols-a cols-b))
  
  (new-data-frame (map (λ: ((name : Label))
                         (cons name
                               (let ((dfa-series (data-frame-series-ref dfa name))
                                     (dfb-series (data-frame-series-ref dfb name))) 
                                 (match (list (series-type dfa-series) (series-type dfb-series))
                                   ((list 'NumericSeries 'IntegerSeries)     (>=/ns/is (data-frame-nseries dfa name)
                                                                                      (data-frame-iseries dfb name)))
                                   ((list 'IntegerSeries 'NumericSeries)    (>=/is/ns (data-frame-iseries dfa name)
                                                                                     (data-frame-nseries dfb name)))
                                   ((list 'IntegerSeries 'IntegerSeries)    (>=/is (data-frame-iseries dfa name)
                                                                                  (data-frame-iseries dfb name)))
                                   ((list 'NumericSeries 'NumericSeries)    (>=/ns (data-frame-nseries dfa name)
                                                                                  (data-frame-nseries dfb name)))
                                   (else (error 'data-frame>=
                                                "Incorrect series type ~a ~a, must be Integer or Numeric."
                                                (series-type dfa-series) (series-type dfb-series)))))))
                         (filter (λ: ((name : Label))
                                   (set-member? matching-cols name))
                                 (data-frame-names dfa)))))


;(: data-frame-abs (DataFrame [#:on (Listof Symbol)] -> DataFrame))
(: data-frame-abs (DataFrame -> DataFrame))
(define (data-frame-abs df)
  (new-data-frame (map (λ: ((name : Label))
                         (cons name
                               (let ((df-series (data-frame-series-ref df name))) 
                                 (match (series-type df-series)
                                   ('NumericSeries (nseries-abs (data-frame-nseries df name)))
                                   ('IntegerSeries (iseries-abs (data-frame-iseries df name)))
                                   (else (error 'data-frame-abs
                                                "Incorrect series type ~a, must be Integer or Numeric."
                                                (series-type df-series)))))))
                       (data-frame-names df))
                  #:index (data-frame-index df)))

; DataFrame.apply(func[, axis, broadcast, ...])	Applies function along input axis of DataFrame.
; (: data-frame-apply axis func)

(define-type ListofGenericType (Listof Any))
(define-predicate ListofGenericType? ListofGenericType)

; DataFrame.applymap(func)	Apply a function to a DataFrame that is intended to operate elementwise, i.e.
(: data-frame-apply (DataFrame (Any -> Any) -> DataFrame))
(define (data-frame-apply data-frame func)
  (new-data-frame 
   (map (λ: ((col : Column))
          (let ([data (series-data (column-series col))])
            (cons (column-heading col)
                  (new-series (map func
                                   (if (vector? data)
                                       (assert (vector->list (assert data vector?)) ListofGenericType?)
                                       (assert (flvector->list (assert data flvector?)) ListofGenericType?)))
                              #:index (data-frame-index data-frame)))))
        (data-frame-explode data-frame))
   #:index (data-frame-index data-frame)))

; DataFrame.aggregate(func[, axis])	Aggregate using callable, string, dict, or list of string/callables
; (: data-frame-aggregate func)

; DataFrame.groupby([by, axis, level, ...])	Group series using mapper (dict or key function, apply given function to group, return result as series) or by a series of columns.
; (: data-frame-groupby [#:by (Listof Symbol)])

; This function consumes two Vectorof Series and two Vectorof
; SeriesBuilder. The types of Series and SeriesBuilder must
; match in the respective indicies.
(: do-filter ((Vectorof Series) (Vectorof SeriesBuilder) (Vectorof Boolean) -> Void))
(define (do-filter cols builders filter-vec)

  (define: col-cnt : Fixnum (vector-length cols))
  (define: df-len   : Fixnum (series-length (vector-ref cols #{0 : Index} )))

  (for ((df-row (in-range df-len)))
       (let ((df-row : Index (assert df-row index?)))
         (when (vector-ref filter-vec df-row)
	      (copy-column-row cols builders df-row)))))

; ***********************************************************

(: data-frame-filter (DataFrame BSeries -> DataFrame))
(define (data-frame-filter data-frame bseries)

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

  (define: df-cols : Columns (data-frame-cols data-frame '()))
  
  ; Get series builders of default length 10 for all columns in fb.
  (define: dest-builders : (Vectorof SeriesBuilder)
    (list->vector (dest-mapping-series-builders (data-frame-description data-frame) 10)))

  (do-filter (src-series df-cols) dest-builders (bseries-data bseries))

  (define: new-series : Columns
    (for/list ([builder (in-vector dest-builders)]
	       [col     (in-list df-cols)])
	      (cons (column-heading col) (series-complete builder))))

  (new-data-frame new-series))

; ***********************************************************

(: data-frame-column-filter (DataFrame (Any -> Boolean) Label -> DataFrame))
(define (data-frame-column-filter data-frame filter-function name)
  (let* ((series : Series (data-frame-series-ref data-frame name))
         (series-filtered : Series (series-filter series filter-function))
         (filtered-index : RFIndex (get-series-index series-filtered))
         (filtered-idxes : (Listof Index) (series-data-idxes-from-predicate series filter-function)))
    (new-data-frame 
     (map (λ: ((col : Column))           
            (cons (column-heading col)
                  ; filter to only values in index                    
                  (assert (series-iloc (column-series col) filtered-idxes) Series?)))
          (data-frame-explode data-frame))
     #:index filtered-index)))

    