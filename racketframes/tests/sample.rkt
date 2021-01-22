;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*    Author: Shubham Kahal
;*    File: tests/integer-series.rkt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket
(require typed/rackunit)

(require "../load/sample.rkt"
         (only-in "../load/schema.rkt"
	  Schema ColumnInfo Schema-headers Schema-SeriesTypes
	  generate-anon-series-names)
         (only-in "../load/delimited.rkt"
	 set-delimiter))

; add these to test file
(define schema-1 (determine-schema-from-sample (list "header1, header2, header3, header4" "hello, world, fizz, buzz") ","))

(check-equal? (Schema-headers schema-1) (list 'header1 'header2 'header3 'header4))

(check-equal? (Schema-SeriesTypes schema-1) (list 'CategoricalSeries 'CategoricalSeries 'CategoricalSeries 'CategoricalSeries))

; this function gets the string split on the delimter (Listof (Listof String)) 
;(transpose-rows-to-cols (list (list "header1" "header2" "header3" "header4") (list "hello" "world" "fizz" "buzz") (list "hello" "world" "fizz" "buzz") (list "hello, world, fizz, buzz") (list "hello, world, fizz, buzz") (list "hello" "world" "fizz" "buzz")))

(check-equal? (transpose-rows-to-cols (list (list "hello" "world") (list "fizz" "buzz"))) '(("hello" "fizz") ("world" "buzz")))

; (list "first|last|gender|yn|char|float" "Louis|Lawson|Male|Y|z|-320102186614.784") needs to become
; (list (list "first" "Louis") (list "last" "Lawson") etc.)

;(determine-schema-from-sample (list "first|last|gender|yn|char|float" "Louis|Lawson|Male|Y|z|-320102186614.784") "|")

;(define schema-2 (determine-schema-from-sample (list "5.7" "generic header" "world" "fizz" "buzz" "1" "2.5") ","))

;(Schema-headers schema-2)

;(Schema-SeriesTypes schema-2)

;(define schema-3 (determine-schema-from-sample (list "generic header" "5.7" "world" "fizz" "buzz" "1" "2.5") ","))

;(Schema-headers schema-3)

;(Schema-SeriesTypes schema-3)

;(define schema-4 (determine-schema-from-sample (list "Numeric Header" "5.7" "2.5") ","))

;(Schema-headers schema-4)

;(Schema-SeriesTypes schema-4)