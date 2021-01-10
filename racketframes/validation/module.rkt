#lang typed/racket

(require RacketFrames
         racket/runtime-path
         typed/db)

(define-runtime-path sample-csv "../sample-csv/")

;******************
;data-frame-mix
;******************
(define columns-mix
  (list
   (cons 'integer-col (new-ISeries (vector 1 2 3 4)
                            (build-index-from-list (list 'a 'b 'c 'd))))
   (cons 'categorical-col (new-CSeries (vector 'hello 'world 'fizz 'buzz)
                                       (build-index-from-list (list 'a 'b 'c 'd))))))

; create new data-frame-mix
(define data-frame-mix (new-data-frame columns-mix))

(data-frame-write-tab data-frame-mix (current-output-port))

; no schema
(define salary-data-frame-csv-no-schema
  (load-csv-file (build-path sample-csv "salary_date.csv") #:schema #f))

(data-frame-head salary-data-frame-csv-no-schema)

(print salary-data-frame-csv-no-schema)

(displayln "DataFrame List of Column Names")
(data-frame-names salary-data-frame-csv-no-schema)

(displayln "DataFrame Dimensions")
(data-frame-dim salary-data-frame-csv-no-schema)

(displayln "DataFrame Description")
(show-data-frame-description (data-frame-description salary-data-frame-csv-no-schema))

(displayln "DataFrame Remove")
(data-frame-head (data-frame-remove salary-data-frame-csv-no-schema (list 'first 'age)))

(displayln "DataFrame Project")
(data-frame-head (data-frame-project salary-data-frame-csv-no-schema (list 'first 'last 'dollar)))

(displayln "DataFrame Replace")
(data-frame-head (data-frame-replace salary-data-frame-csv-no-schema (cons 'salary (new-CSeries (make-vector 200 '$0.00) #f))))

(displayln "DataFrame Replace Non Existent Column")
(data-frame-head (data-frame-replace salary-data-frame-csv-no-schema (cons 'dollar (new-CSeries (make-vector 200 '$0.00) #f))))

(displayln "DataFrame Extend")
(data-frame-head (data-frame-extend salary-data-frame-csv-no-schema (cons 'state (new-CSeries (make-vector 200 'CA) #f))))

(displayln "DataFrame Operations")
(define columns-integer-1
  (list 
   (cons 'col1 (new-ISeries (vector 1 2 3 4) #f))
   (cons 'col2 (new-ISeries (vector 5 6 7 8) #f))
   (cons 'col3 (new-ISeries (vector 9 10 11 12) #f))
   (cons 'col4 (new-ISeries (vector 21 22 23 24) #f))))

(define columns-integer-2
  (list 
   (cons 'col1 (new-ISeries (vector 1 2 3 4) #f))
   (cons 'col2 (new-ISeries (vector 25 26 27 28) #f))
   (cons 'col3 (new-ISeries (vector 29 30 31 32) #f))
   (cons 'col4 (new-ISeries (vector 1 2 3 4) #f))))

; create new data-frame-integer-1
(define data-frame-integer-1 (new-data-frame columns-integer-1))

; create new data-frame-integer-2
(define data-frame-integer-2 (new-data-frame columns-integer-2))

(displayln "Sample DataFrames")
(data-frame-write-tab data-frame-integer-1 (current-output-port))

(data-frame-write-tab data-frame-integer-2 (current-output-port))
(display "\n")

(displayln "data-frame+")
(data-frame-write-tab (data-frame+ data-frame-integer-1 data-frame-integer-2) (current-output-port))
(display "\n")

(displayln "data-frame-")
(data-frame-write-tab (data-frame- data-frame-integer-1 data-frame-integer-2) (current-output-port))
(display "\n")

(displayln "data-frame*")
(data-frame-write-tab (data-frame* data-frame-integer-1 data-frame-integer-2) (current-output-port))
(display "\n")

(displayln "data-frame/")
(data-frame-write-tab (data-frame/ data-frame-integer-1 data-frame-integer-2) (current-output-port))
(display "\n")

(displayln "data-frame%")
(data-frame-write-tab (data-frame% data-frame-integer-1 data-frame-integer-2) (current-output-port))
(display "\n")

(displayln "data-frame-r")
(data-frame-write-tab (data-frame-r data-frame-integer-1 data-frame-integer-2) (current-output-port))
(display "\n")


(displayln "DataFrame Join Operations")
(define columns-integer
  (list 
   (cons 'col1 (new-ISeries (vector 1 2 3 4) #f))
   (cons 'col2 (new-ISeries (vector 5 6 7 8) #f))
   (cons 'col3 (new-ISeries (vector 9 10 11 12) #f))))

(define columns-categorical
  (list 
   (cons 'col1 (new-CSeries (vector 'a 'b 'c 'd 'e) #f))
   (cons 'col2 (new-CSeries (vector 'e 'f 'g 'h 'i) #f))
   (cons 'col3 (new-CSeries (vector 'j 'k 'l 'm 'n) #f))))

; create new data-frame-integer
(define data-frame-integer (new-data-frame columns-integer))

; create new data-frame-categorical
(define data-frame-categorical (new-data-frame columns-categorical))

(displayln "DataFrame Left Join")
(data-frame-write-tab (data-frame-join-left data-frame-integer data-frame-categorical) (current-output-port))
(display "\n")

(displayln "DataFrame Right Join")
(data-frame-write-tab (data-frame-join-right data-frame-integer-1 data-frame-integer-2 #:on (list 'col1)) (current-output-port))
(display "\n")

(displayln "DataFrame Right Join")
(data-frame-write-tab (data-frame-join-right data-frame-integer-1 data-frame-integer-2 #:on (list 'col2)) (current-output-port))
(display "\n")

(define columns-mixed-5
  (list 
   (cons 'col1 (new-ISeries (vector 1 2 3 4) #f))
   (cons 'col2 (new-CSeries (vector 'a 'b 'c 'd) #f))
   (cons 'col3 (new-ISeries (vector 21 22 23 24) #f))))

(define columns-mixed-6
  (list 
   (cons 'col1 (new-ISeries (vector 11 21 31 41) #f))
   (cons 'col2 (new-CSeries (vector 'a 'b 'g 'd) #f))
   (cons 'col3 (new-ISeries (vector 22 22 23 24) #f))))

; create new data-frame-mixed-5
(define data-frame-mixed-5 (new-data-frame columns-mixed-5))

; create new data-frame-mixed-6
(define data-frame-mixed-6 (new-data-frame columns-mixed-6))'

(displayln "DataFrame Mixed")
(data-frame-write-tab data-frame-mixed-5 (current-output-port))

(displayln "DataFrame Mixed")
(data-frame-write-tab data-frame-mixed-6 (current-output-port))

(displayln "DataFrame Inner Join")
(data-frame-write-tab (data-frame-join-inner data-frame-mixed-5  data-frame-mixed-6 #:on (list 'col2)) (current-output-port))

(display "\n")

(displayln "DataFrame Outer Join")
(data-frame-write-tab (data-frame-join-outer data-frame-mixed-5  data-frame-mixed-6 #:on (list 'col2)) (current-output-port))

(displayln "DataFrame iloc")

(define columns-integer-labeled
  (list 
   (cons 'col1 (new-ISeries (vector 1 2 3 4)
                            (build-index-from-list (list 'a 'b 'c 'd))))
   (cons 'col2 (new-ISeries (vector 5 6 7 8)
                            (build-index-from-list (list 'e 'f 'g 'h))))
   (cons 'col3 (new-ISeries (vector 9 10 11 12)
                            (build-index-from-list (list 'i 'j 'k 'l))))))

; create new data-frame-integer
(define data-frame-integer-labeled (new-data-frame columns-integer-labeled))

(data-frame-write-tab data-frame-integer-labeled
                      (current-output-port))

(displayln "data-frame-loc")
(data-frame-write-tab
 (assert (data-frame-loc data-frame-integer-labeled (list 'i 'k) (list 'col3)) DataFrame?)
 (current-output-port))

(define columns-integer-labeled-2
  (list 
   (cons 'col1 (new-ISeries (vector 1 2 3 4)
                            (build-index-from-list (list 'a 'b 'c 'd))))
   (cons 'col2 (new-ISeries (vector 5 6 7 8)
                            (build-index-from-list (list 'a 'b 'c 'd))))
   (cons 'col3 (new-ISeries (vector 9 10 11 12)
                            (build-index-from-list (list 'a 'b 'c 'd))))))

; create new data-frame-integer
(define data-frame-integer-labeled-2 (new-data-frame columns-integer-labeled-2))

(displayln "data-frame-loc-2")
(data-frame-write-tab
 (assert (data-frame-loc data-frame-integer-labeled-2 (list 'b 'c 'd) (list 'col2 'col3)) DataFrame?)
 (current-output-port))

(set! data-frame-integer-labeled (data-frame-set-index data-frame-integer-labeled (list 'a 'b 'c 'd)))

(data-frame-write-tab
 (assert (data-frame-loc data-frame-integer-labeled (list 'b 'c 'd) (list 'col2 'col3)) DataFrame?)
 (current-output-port))

(data-frame-write-tab
 (assert (data-frame-iloc data-frame-integer-labeled (list 1 2 3) (list 0 1)) DataFrame?)
 (current-output-port))

(println "Hash list")
(hash->list (LabelIndex-index data-frame-integer-labeled))

(iseries-data (assert (data-frame-iloc data-frame-integer-labeled (list 1 2 3) 1) ISeries?))

(gen-series-data (assert (data-frame-iloc data-frame-integer-labeled 3 1) GenSeries?))

(data-frame-write-tab
 (assert (data-frame-iloc-label data-frame-integer-labeled (list 1 2 3) (list 'col1 'col2)) DataFrame?)
 (current-output-port))

(gen-series-data (assert (data-frame-iloc-label data-frame-integer-labeled 1 (list 'col1 'col2)) GenSeries?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Dataframe Load Test Cases;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define salary-date-schema (Schema #t (list (ColumnInfo 'first 'CATEGORICAL) (ColumnInfo 'last 'CATEGORICAL)
                                       (ColumnInfo 'age 'INTEGER) (ColumnInfo 'dollar 'CATEGORICAL)
                                       (ColumnInfo 'phone 'CATEGORICAL) (ColumnInfo 'join_date 'DATETIME))))

(define salary-no-date-schema (Schema #t (list (ColumnInfo 'first 'CATEGORICAL) (ColumnInfo 'last 'CATEGORICAL)
                                       (ColumnInfo 'age 'INTEGER) (ColumnInfo 'dollar 'CATEGORICAL)
                                       (ColumnInfo 'phone 'CATEGORICAL))))

(define salary-datetime-schema (Schema #t (list (ColumnInfo 'first 'CATEGORICAL) (ColumnInfo 'last 'CATEGORICAL)
                                       (ColumnInfo 'age 'INTEGER) (ColumnInfo 'dollar 'CATEGORICAL)
                                       (ColumnInfo 'phone 'CATEGORICAL) (ColumnInfo 'join_datetime 'DATETIME))))

(define random-demographic-schema (Schema #t (list (ColumnInfo 'first 'CATEGORICAL) (ColumnInfo 'last 'CATEGORICAL)
                                                   (ColumnInfo 'gender 'CATEGORICAL) (ColumnInfo 'yn 'CATEGORICAL)
                                                   (ColumnInfo 'char 'GENERIC) (ColumnInfo 'float 'NUMERIC))))

; read csv
(define salary-data-frame-csv-schema
  (load-csv-file (build-path sample-csv "salary_date.csv") #:schema salary-date-schema))

(data-frame-head salary-data-frame-csv-schema)

(define salary-no-date-data-frame-csv-schema
  (load-csv-file (build-path sample-csv "salary_no_date.csv") #:schema salary-no-date-schema))

(data-frame-head salary-no-date-data-frame-csv-schema)

(define salary-datetime-data-frame-csv-schema
  (load-csv-file (build-path sample-csv "salary_datetime.csv") #:schema salary-datetime-schema))

(data-frame-head salary-datetime-data-frame-csv-schema)

(define salary-datetime-date-data-frame-csv-schema
  (load-csv-file (build-path sample-csv "salary_datetime_date.csv") #:schema salary-datetime-schema))

(data-frame-head salary-datetime-date-data-frame-csv-schema)


; read delimited
(define random-demographic-data-frame-delimited
  (load-delimited-file (build-path sample-csv "random_demographic.csv")
                       "|" #:schema random-demographic-schema))

(data-frame-head random-demographic-data-frame-delimited)

(series-data (data-frame-series random-demographic-data-frame-delimited 'char))

; no schema
(define random-demographic-data-frame-delimited-no-schema
  (load-delimited-file (build-path sample-csv "random_demographic.csv") "|" #:schema #f))

(data-frame-head random-demographic-data-frame-delimited-no-schema)

(define data-frame-from-sql-genres
  (data-frame-from-sql (sqlite3-connect #:database "db/chinook.db")
                       #f "SELECT * FROM genres" empty))

(data-frame-head data-frame-from-sql-genres)

(define data-frame-from-sql-customers
  (data-frame-from-sql (sqlite3-connect #:database "db/chinook.db")
                       #f "SELECT * FROM customers" empty))

(data-frame-head data-frame-from-sql-customers)