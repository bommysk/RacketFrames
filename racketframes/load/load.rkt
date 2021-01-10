#lang typed/racket

(provide:
 [determine-schema (Path-String Integer String -> Schema)]
 [load-csv-file (Path-String [#:schema (Option Schema)] -> DataFrame)]
 [load-delimited-file (Path-String String [#:schema (Option Schema)] -> DataFrame)]
 [data-frame-from-sql (Connection Boolean String (Listof Any) -> DataFrame)])

(require
  typed/db
 racket/match
 (only-in "../util/list.rkt"
	  zip)
 (only-in "schema.rkt"
	  generate-anon-series-names
	  Schema ColumnInfo SeriesTypes Schema-has-headers
	  Schema-SeriesTypes Schema-headers)
 (only-in "../data-frame/series-builder.rkt"
	  SeriesBuilder)
 (only-in "../data-frame/indexed-series.rkt"
          ListofAny?)
 (only-in "../data-frame/generic-series-builder.rkt"
	  new-GenSeriesBuilder
	  GenSeriesBuilder
	  GenSeriesBuilder?
	  complete-GenSeriesBuilder)
 (only-in "../data-frame/integer-series-builder.rkt"
	  new-ISeriesBuilder
	  ISeriesBuilder
	  ISeriesBuilder?
	  complete-ISeriesBuilder)
 (only-in "../data-frame/boolean-series-builder.rkt"
	  new-BSeriesBuilder
	  BSeriesBuilder
	  BSeriesBuilder?
	  complete-BSeriesBuilder)
 (only-in "../data-frame/numeric-series-builder.rkt"
	  new-NSeriesBuilder
	  NSeriesBuilder
	  NSeriesBuilder?
	  complete-NSeriesBuilder)
 (only-in "../data-frame/categorical-series-builder.rkt"
	  new-CSeriesBuilder
	  CSeriesBuilder
	  CSeriesBuilder?
	  complete-CSeriesBuilder
	  append-CSeriesBuilder)
 (only-in "../data-frame/datetime-series-builder.rkt"
	  new-DatetimeSeriesBuilder
	  DatetimeSeriesBuilder
	  DatetimeSeriesBuilder?
	  complete-DatetimeSeriesBuilder
	  append-DatetimeSeriesBuilder)
 (only-in "../data-frame/series-description.rkt"
	  Series)
 (only-in "../data-frame/data-frame.rkt"
	  DataFrame
	  new-data-frame
          data-frame-explode)
 (only-in "../data-frame/data-frame-print.rkt"
          data-frame-write-tab
          data-frame-head)
 "data-frame-builder.rkt"
 (only-in "delimited-common.rkt"
	  sample-formatted-file
	  check-data-file-exists
          read-sql-query)
 (only-in "schema.rkt"
	  Schema)
 (only-in "sample.rkt"
	  determine-schema-from-sample
          determine-schema-from-sql-sample)
 (only-in "csv-delimited.rkt"
	  read-csv-file)
 (only-in "delimited.rkt"
	  read-delimited-file
          read-sql-results))

(struct: QueryResult ([headers : (Listof String)]
                      [rows : (Listof (Vectorof Any))]
                      [num-rows : Index]))

(: new-DataFrameBuilder-from-Schema (Schema -> DataFrameBuilder))
(define (new-DataFrameBuilder-from-Schema schema)

  (: determine-SeriesBuilder (SeriesTypes -> SeriesBuilder))
  (define (determine-SeriesBuilder stypes)
    (match stypes
      ['GENERIC (new-GenSeriesBuilder)]
      ['CATEGORICAL (new-CSeriesBuilder)]
      ['INTEGER     (new-ISeriesBuilder)]
      ['NUMERIC     (new-NSeriesBuilder)]
      ['BOOLEAN     (new-BSeriesBuilder)]
      ['DATETIME    (new-DatetimeSeriesBuilder)]))

  (DataFrameBuilder ((inst map SeriesBuilder SeriesTypes)
		 determine-SeriesBuilder
		 (Schema-SeriesTypes schema))))

(: complete-SeriesBuilders (DataFrameBuilder -> (Listof Series)))
(define (complete-SeriesBuilders frame-builder)
  (map (λ: ((builder : SeriesBuilder))
         (cond
           [(GenSeriesBuilder? builder)
            (complete-GenSeriesBuilder builder)]
           [(CSeriesBuilder? builder)
            (complete-CSeriesBuilder builder)]
           [(ISeriesBuilder? builder)
            (complete-ISeriesBuilder builder)]
           [(BSeriesBuilder? builder)
            (complete-BSeriesBuilder builder)]
           [(NSeriesBuilder? builder)
            (complete-NSeriesBuilder builder)]
           [(DatetimeSeriesBuilder? builder)
            (complete-DatetimeSeriesBuilder builder)]
           [else (error "Inconsistent DataFrameBuilder")]))
       (DataFrameBuilder-builders frame-builder)))

(: anon-headers (Integer -> (Listof Symbol)))
(define (anon-headers cnt)
  (map string->symbol (generate-anon-series-names cnt)))

(: make-data-frame (Schema DataFrameBuilder -> DataFrame))
(define (make-data-frame schema builder)
  (let ((cols (complete-SeriesBuilders builder)))
    (let ((headers (if (Schema-has-headers schema)
		       (Schema-headers schema)
		       (anon-headers (length cols)))))
      (new-data-frame ((inst zip Symbol Series) headers cols)))))

(: schema-if-needed ((Option Schema) Path-String (Option String) -> Schema))
(define (schema-if-needed schema path delim)
  (define SAMPLE-SIZE 20)
  (if schema schema (determine-schema path SAMPLE-SIZE (if delim delim ","))))

; delimiter must be specified by user if no schema provided, need to still do this
(: load-csv-file (Path-String [#:schema (Option Schema)] -> DataFrame))
(define (load-csv-file path #:schema [schema #f])
  (let* ((schema (schema-if-needed schema path #f)))
    (make-data-frame schema (read-csv-file path
                                           (Schema-has-headers schema)
                                           (new-DataFrameBuilder-from-Schema schema)))))

(: load-delimited-file (Path-String String [#:schema (Option Schema)] -> DataFrame))
(define (load-delimited-file path delim #:schema [schema #f])
  (let* ((schema (schema-if-needed schema path delim)))
    (make-data-frame schema (read-delimited-file path
                                                 (Schema-has-headers schema)
                                                 (new-DataFrameBuilder-from-Schema schema)
                                                 delim))))

(: determine-schema (Path-String Integer String -> Schema))
(define (determine-schema fpath cnt delim)
  (check-data-file-exists fpath)
  (determine-schema-from-sample (sample-formatted-file fpath cnt) delim))

(: determine-schema-sql (QueryResult -> Schema))
(define (determine-schema-sql query-result)
  (determine-schema-from-sql-sample (QueryResult-headers query-result) (QueryResult-rows query-result)))

(: get-query-result (Connection String (Listof Any) -> QueryResult))
(define (get-query-result conn sql-query params)
  ;(mysql-connect #:socket "/Applications/MAMP/tmp/mysql/mysql.sock"
   ;                                  #:database "database_name"
    ;                                 #:user "root"
     ;                                #:password "root")

  (define result (query conn sql-query))

  (define headers
    (for/list: : (Listof Any) ([hdr (rows-result-headers (assert result rows-result?))])
      (cond [(assq 'name (assert hdr ListofAny?)) => cdr]
            [else "unnamed"])))
    
  (define rows (rows-result-rows (assert result rows-result?)))

  ;(println rows)
  (define num-rows (length rows))

  (QueryResult (cast headers (Listof String)) (cast rows (Listof (Vectorof Any))) num-rows))

; Create a data frame from the result of running SQL-QUERY on the database DB
; with the supplied PARAMS.  SQL-QUERY can be either a string or a
; virtual-query object.  Each column from the result set will become a series
; in the data frame, sql-null values will be converted to #f.
(: data-frame-from-sql (Connection Boolean String (Listof Any) -> DataFrame))
(define (data-frame-from-sql conn schema-test sql-query params)

  (define query-result (get-query-result conn sql-query params)) 

  ;; If query returned 0 rows, don't add any series to the data frame
  (when (> 1 0)
    (let ((schema (determine-schema-sql query-result))) ; determine schema from first row of data, fill this in later
      ; make data-frame by passing in the schema and associated data-frame-builder
           (make-data-frame schema (read-sql-results (QueryResult-headers query-result)
                                                   (QueryResult-rows query-result)
                                                   (new-DataFrameBuilder-from-Schema schema)))))

  ; return empty dataframe if query returned 0 results
  )

;tests
;(define data-frame-from-sql-invoices (data-frame-from-sql (sqlite3-connect #:database "/Users/shubhamkahal/Documents/RacketFrames/dataframe/validation/db/chinook.db") #f "SELECT * FROM invoices" empty))

;(data-frame-head data-frame-from-sql-invoices)

;(define data-frame-from-sql-customers (data-frame-from-sql (sqlite3-connect #:database "/Users/shubhamkahal/Documents/RacketFrames/dataframe/validation/db/chinook.db") #f "SELECT * FROM customers" empty))

;(data-frame-head data-frame-from-sql-customers)