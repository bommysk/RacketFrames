;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*    Author: Shubham Kahal
;*    File: data-frame-print.rkt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket
(require typed/rackunit)
(require racket/struct)
(require racket/pretty)

; ***********************************************************
; This module provides data frame printing functionality.
; ***********************************************************

; **************************
; Test cases are at bottom
; of file.
; **************************

; ***********************************************************
; Provide functions in this file to other files.

(provide:
 [data-frame-write-delim (DataFrame Output-Port [#:heading Boolean] -> Void)]
 [data-frame-head (case-> (DataFrame -> Void)
		     (DataFrame (Option Index) -> Void))])

; ***********************************************************

; ***********************************************************

(require
 racket/match
 (only-in grip/data/format
	  ~a ~r)
 (only-in "types.rkt"
	  Dim Dim-rows)
 (only-in "indexed-series.rkt"
	  Label)
 (only-in "series-description.rkt"
	  Series series-type)
 (only-in "data-frame.rkt"
	  DataFrame new-data-frame data-frame-names
	  Column Columns column-heading column-series
	  data-frame-cseries data-frame-explode data-frame-dim
	  DataFrameDescription DataFrameDescription-series
	  show-data-frame-description data-frame-description)
(only-in "generic-series.rkt"
	  new-GenSeries GenSeries GenSeries? gen-series-iref)
 (only-in "integer-series.rkt"
	  new-ISeries ISeries ISeries? iseries-iref)
 (only-in "boolean-series.rkt"
	  new-BSeries BSeries BSeries? bseries-iref)
 (only-in "numeric-series.rkt"
	  NSeries NSeries? nseries-iref)
 (only-in "categorical-series.rkt"
	  cseries-referencer cseries-length cseries-iref
	  CSeries CSeries?))

; ***********************************************************

; ***********************************************************

(define WIDTH 15)

(define-type DataFrameRowFormatter (DataFrame Natural -> String))

; ***********************************************************

; ***********************************************************

; Test Cases

;******************
;data-frame-integer
;******************
; will define parse to automatically build this columns structure
(define columns-integer
  (list 
   (cons 'col1 (new-ISeries (vector 1 2 3 4)
                            (list 'a 'b 'c 'd)))
   (cons 'col2 (new-ISeries (vector 5 6 7 8)
                            (list 'e 'f 'g 'h)))
   (cons 'col3 (new-ISeries (vector 9 10 11 12)
                            (list 'i 'j 'k 'l)))))

; create new data-frame-integer
(define data-frame-integer (new-data-frame columns-integer))

(data-frame-write-delim data-frame-integer)
