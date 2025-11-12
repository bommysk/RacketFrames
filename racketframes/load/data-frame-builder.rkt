#lang typed/racket/base

(provide 
 (struct-out DataFrameBuilder)
 append-data-fields
 append-sql-data-fields)

(require 
 (only-in "../data-frame/series-builder.rkt"
          SeriesBuilder))

(struct: DataFrameBuilder ([builders : (Listof SeriesBuilder)]))

(: check-all-data-processed ((Listof Any) (Listof Any) -> Boolean))
(define (check-all-data-processed l1 l2)
  (and (null? l1) (null? l2)))

(: append-data-fields ((Listof (String -> Void)) (Listof String) -> Boolean))
(define (append-data-fields appenders fields)
  (if (or (null? appenders)
	  (null? fields))      
      (check-all-data-processed appenders fields)
      (begin
        ; call appropriate appender on each field, different appender per column
	((car appenders) (car fields))
	(append-data-fields (cdr appenders) (cdr fields)))))

(: append-sql-data-fields ((Listof (Any -> Void)) (Listof Any) -> Boolean))
(define (append-sql-data-fields appenders fields)
  (if (or (null? appenders)
	  (null? fields))      
      (check-all-data-processed appenders fields)
      (begin
        ; call appropriate appender on each field, different appender per column
	((car appenders) (car fields))
	(append-sql-data-fields (cdr appenders) (cdr fields)))))
