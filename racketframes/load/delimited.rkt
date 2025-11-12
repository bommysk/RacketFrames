#lang typed/racket/base

(provide:
 [set-delimiter (String -> LineParser)]
 [read-delimited-file (Path-String Boolean DataFrameBuilder String -> DataFrameBuilder)]
 [read-sql-results ((Listof String) (Listof (Vectorof Any)) DataFrameBuilder -> DataFrameBuilder)])

(require
 (only-in "types.rkt"
	  LineParser)
 (only-in "delimited-common.rkt"
	  read-formatted-file
          read-sql-query)
 (only-in "data-frame-builder.rkt"
	  DataFrameBuilder))

(require/typed racket/string
	       [string-trim (String -> String)]
	       [string-split (String (U String Regexp) -> (Listof String))])

(: set-delimiter (String -> LineParser))
(define (set-delimiter delim)
  (lambda ([str : String])
    (map string-trim (string-split str delim))))

(: read-delimited-file (Path-String Boolean DataFrameBuilder String -> DataFrameBuilder))
(define (read-delimited-file fpath header? data-frame-builder delim)
  (begin (read-formatted-file fpath header? data-frame-builder (set-delimiter delim))
         data-frame-builder))

(: read-sql-results ((Listof String) (Listof (Vectorof Any)) DataFrameBuilder -> DataFrameBuilder))
(define (read-sql-results headers rows data-frame-builder)
  (begin (read-sql-query headers
                         rows
                         data-frame-builder)
         data-frame-builder))