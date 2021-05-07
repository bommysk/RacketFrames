;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*    Author: Shubham Kahal
;*    File: test/data-frame-print.rkt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket
;#lang typed/racket/no-check
(require typed/rackunit)
(require racket/fixnum)

; **************************
; Test cases are at bottom
; of file.
; **************************

; ***********************************************************

; ***********************************************************

(require
 (only-in "../data-frame/data-frame.rkt"
	  DataFrame new-data-frame data-frame-names
	  Column Columns column-heading column-series
	  data-frame-cseries data-frame-explode data-frame-dim
	  DataFrameDescription DataFrameDescription-series
	  show-data-frame-description data-frame-description)
 (only-in "../data-frame/indexed-series.rkt"
          build-index-from-labels build-index-from-list build-index-from-sequence)
 (only-in "../data-frame/data-frame-print.rkt"
          data-frame-write-delim data-frame-write-csv data-frame-write-json data-frame-write-json-file)
 (only-in "../data-frame/generic-series.rkt"
         new-GenSeries)
 (only-in "../data-frame/integer-series.rkt"
	  new-ISeries)
 (only-in "../data-frame/boolean-series.rkt"
	  new-BSeries)
 (only-in "../data-frame/numeric-series.rkt"
	  new-NSeries)
 (only-in "../data-frame/categorical-series.rkt"
	  new-CSeries)
 (only-in "../data-frame/datetime-series.rkt"
	  new-DatetimeSeries)
 (only-in "../data-frame/date-series.rkt"
	  new-DateSeries))

; ***********************************************************

; ***********************************************************

; Test Cases

;******************
;data-frame-integer
;******************
; will define parse to automatically build this columns structure
(define columns-integer
  (list 
   (cons 'col1 (new-ISeries (fxvector 1 2 3 4)
                            #:index (list 'a 'b 'c 'd)))
   (cons 'col2 (new-ISeries (fxvector 5 6 7 8)
                            #:index (list 'e 'f 'g 'h)))
   (cons 'col3 (new-ISeries (fxvector 9 10 11 12)
                            #:index (list 'i 'j 'k 'l)))))

; create new data-frame-integer
(define data-frame-integer (new-data-frame columns-integer))

(data-frame-write-delim data-frame-integer)

(data-frame-write-delim data-frame-integer #:delim ",")

(data-frame-write-csv data-frame-integer "test_out_data_frame_integer.csv")

(data-frame-write-delim data-frame-integer #:delim "  ")

(define columns-float
  (list 
   (cons 'col1 (new-NSeries (vector 1.5 2.5 3.5 4.5)
                            #:index (build-index-from-list (list 'a 'b 'c 'd))))
   (cons 'col2 (new-NSeries (vector 5.5 6.5 7.5 8.5)
                            #:index (build-index-from-list (list 'e 'f 'g 'h))))
   (cons 'col3 (new-NSeries (vector 9.5 10.5 11.5 12.5)
                            #:index (build-index-from-list (list 'i 'j 'k 'l))))))

; create new data-frame-float
(define data-frame-float (new-data-frame columns-float))

(data-frame-write-delim data-frame-float)

(data-frame-write-delim data-frame-float #:delim ",")

(data-frame-write-csv data-frame-float "test_out_data_frame_float.csv")

(define columns-mix
  (list
   (cons 'integer-col (new-ISeries (fxvector 1 2 3 4)
                            #:index (build-index-from-list (list 'a 'b 'c 'd))))
   (cons 'float-col (new-NSeries (vector 1.5 2.5 3.5 4.5)
                            #:index (build-index-from-list (list 'a 'b 'c 'd))))
   (cons 'categorical-col (new-CSeries (vector 'hello 'world 'fizz 'buzz)))))

; create new data-frame-mix
(define data-frame-mix (new-data-frame columns-mix))

(data-frame-write-delim data-frame-mix)

(data-frame-write-delim data-frame-mix #:delim ",")

(data-frame-write-csv data-frame-mix "test_out_data_frame_mix.csv")

(data-frame-write-json data-frame-mix)

(data-frame-write-json-file data-frame-mix "test_out_data_frame_mix.json")

(define columns-mix-larger
  (list
   (cons 'integer-col (new-ISeries (fxvector 1 2 3 4)
                            #:index (build-index-from-list (list 'a 'b 'c 'd))))
   (cons 'float-col (new-NSeries (vector 1.5 2.5 3.5 4.5)
                            #:index (build-index-from-list (list 'a 'b 'c 'd))))
   (cons 'categorical-col (new-CSeries (vector 'hello 'world 'fizz 'buzz)))
   (cons 'col1 (new-NSeries (vector 1.5 2.5 3.5 4.5)
                            #:index (build-index-from-list (list 'a 'b 'c 'd))))
   (cons 'col2 (new-NSeries (list 5.5 6.5 7.5 8.5)
                            #:index (build-index-from-list (list 'e 'f 'g 'h))))
   (cons 'col3 (new-NSeries (list 9.5 10.5 11.5 12.5)
                            #:index (build-index-from-list (list 'i 'j 'k 'l))))))

(define data-frame-mix-larger (new-data-frame columns-mix-larger))

(data-frame-write-json data-frame-mix-larger)

(data-frame-write-json-file data-frame-mix-larger "test_out_data_frame_mix_larger.json")
