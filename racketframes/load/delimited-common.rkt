#lang typed/racket

(provide:
 [check-data-file-exists (Path-String -> Void)]
 [sample-formatted-file (Path-String Integer -> (Listof String))]
 [data-frame-builder-appenders (DataFrameBuilder -> (Listof (Line -> Void)))]
 [read-formatted-file (Path-String Boolean DataFrameBuilder LineParser -> (Listof Boolean))]
 [read-sql-query ((Listof String) (Listof (Vectorof Any)) DataFrameBuilder -> (Listof Boolean))])

(require
 (only-in "../data-frame/series-builder.rkt"
	  SeriesBuilder)
 (only-in "../data-frame/indexed-series.rkt"
          Datetime?)
 (only-in "../data-frame/generic-series-builder.rkt"
	  GenSeriesBuilder GenSeriesBuilder?
	  append-GenSeriesBuilder)
 (only-in "../data-frame/categorical-series-builder.rkt"
	  CSeriesBuilder CSeriesBuilder?
	  append-CSeriesBuilder)
 (only-in "../data-frame/integer-series-builder.rkt"
	  ISeriesBuilder ISeriesBuilder?
	  append-ISeriesBuilder)
 (only-in "../data-frame/boolean-series-builder.rkt"
	  BSeriesBuilder BSeriesBuilder?
	  append-BSeriesBuilder)
 (only-in "../data-frame/numeric-series-builder.rkt"
	  NSeriesBuilder NSeriesBuilder?
	  append-NSeriesBuilder)
 (only-in "../data-frame/datetime-series-builder.rkt"
	  DatetimeSeriesBuilder DatetimeSeriesBuilder?
	  append-DatetimeSeriesBuilder)
 (only-in "data-frame-builder.rkt"	  
	  DataFrameBuilder DataFrameBuilder-builders
	  append-data-fields append-sql-data-fields)
 (only-in "../util/datetime/types.rkt"
          Datetime Date)
  (only-in "../util/datetime/parse.rkt"
          parse-date parse-datetime is-valid-date? is-valid-datetime?)
 (only-in "schema.rkt"
	  Schema)
 (only-in "types.rkt"
	  LineParser Line Line?))

(: data-frame-builder-appenders (DataFrameBuilder -> (Listof (Line -> Void))))
(define (data-frame-builder-appenders data-frame-builder)
  (map (λ: ((builder : SeriesBuilder))
         (cond
           [(GenSeriesBuilder? builder)
            (λ: ((str : String))
              (append-GenSeriesBuilder builder str))]
           [(CSeriesBuilder? builder)
            (λ: ((str : String))
              (append-CSeriesBuilder builder str))]
           [(ISeriesBuilder? builder)
            (λ: ((str : String))
              (append-ISeriesBuilder builder str))]
           [(BSeriesBuilder? builder)
            (λ: ((str : String))
              (append-BSeriesBuilder builder str))]
           [(NSeriesBuilder? builder)
            (λ: ((str : String))
              (append-NSeriesBuilder builder str))]
           [(DatetimeSeriesBuilder? builder)
            (λ: ((str : String))
              (append-DatetimeSeriesBuilder builder str))]
           [else (λ: ((str : String)) (void))]))
       (DataFrameBuilder-builders data-frame-builder)))

(: data-frame-builder-sql-appenders (DataFrameBuilder -> (Listof (Any -> Void))))
(define (data-frame-builder-sql-appenders data-frame-builder)
  (map (λ: ((builder : SeriesBuilder))
         (cond
           [(GenSeriesBuilder? builder)
            (λ: ((val : Any))
              (append-GenSeriesBuilder builder val))]
           [(CSeriesBuilder? builder)
            (λ: ((val : Any))
              (append-CSeriesBuilder builder (if (string? val) (string->symbol val) (assert val symbol?))))]
           [(ISeriesBuilder? builder)
            (λ: ((val : Any))
              (append-ISeriesBuilder builder (assert val fixnum?)))]
           [(BSeriesBuilder? builder)
            (λ: ((val : Any))
              (append-BSeriesBuilder builder (assert val boolean?)))]
           [(NSeriesBuilder? builder)
            (λ: ((val : Any))
              (append-NSeriesBuilder builder (assert val flonum?)))]
           [(DatetimeSeriesBuilder? builder)
            (λ: ((val : Any))
              (let* ([trimmed-val : String (string-trim (assert val string?))]
                     [dt : Datetime (if (not (parse-datetime trimmed-val)) (assert (parse-date trimmed-val)) (assert (parse-datetime trimmed-val)))])
                (append-DatetimeSeriesBuilder builder (assert dt Datetime?))))]
           [else (λ: ((val : Any)) (void))]))
       (DataFrameBuilder-builders data-frame-builder)))


(: check-data-file-exists (Path-String -> Void))
(define (check-data-file-exists path)
  (unless (file-exists? path)
	  (error (format "File not found: ~s" path))))

(: read-formatted-file (Path-String Boolean DataFrameBuilder LineParser -> (Listof Boolean)))
(define (read-formatted-file path headers? data-frame-builder line-parser)
  (check-data-file-exists path)
  (let* ((file-lines (if headers? (cdr (file->lines path)) (file->lines path))))
    (map (λ: ((line : Line))
           (append-data-fields (data-frame-builder-appenders data-frame-builder) (line-parser line))) file-lines)))

(: read-sql-query ((Listof String) (Listof (Vectorof Any)) DataFrameBuilder -> (Listof Boolean)))
(define (read-sql-query headers rows data-frame-builder)
  (map (λ: ((row : (Vectorof Any)))
         (append-sql-data-fields (data-frame-builder-sql-appenders data-frame-builder) (vector->list row))) rows))

(: sample-formatted-file (Path-String Integer -> (Listof String)))
(define (sample-formatted-file path cnt)
  (check-data-file-exists path)
  (take (file->lines path) cnt))
