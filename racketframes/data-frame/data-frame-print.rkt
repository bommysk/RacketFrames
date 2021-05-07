;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*    Author: Shubham Kahal
;*    File: data-frame-print.rkt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket
(require typed/rackunit)
(require racket/struct)
(require racket/pretty)

(require
 (only-in typed/racket/date
          date->string))
 

; ***********************************************************
; This module provides data frame printing functionality.
; ***********************************************************

; ***********************************************************
; Provide functions in this file to other files.

(provide:
 [data-frame-write-delim (DataFrame [#:output-port Output-Port] [#:heading Boolean] [#:delim String] -> Void)]
 [data-frame-write-csv (DataFrame Path-String -> Void)]
 [data-frame-write-json (DataFrame [#:output-port Output-Port] -> Void)]
 [data-frame-write-json-file (DataFrame Path-String -> Void)]
 [data-frame-head (case-> (DataFrame -> Void)
		     (DataFrame (Option Index) -> Void))])

; ***********************************************************

; ***********************************************************

(require
 racket/match
 (only-in "../util/format.rkt"
	  ~a)
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
 (only-in "datetime-series.rkt"
	  DatetimeSeries DatetimeSeries? datetime-series-iref)
 (only-in "date-series.rkt"
	  DateSeries DateSeries? date-series-iref)
 (only-in "categorical-series.rkt"
	  cseries-referencer cseries-length cseries-iref
	  CSeries CSeries?)
 (only-in "../util/datetime/format.rkt"
          datetime->string))

; ***********************************************************

; ***********************************************************

(define WIDTH 15)

(define-type DataFrameRowFormatter (DataFrame Natural -> String))

; ***********************************************************

; ***********************************************************

(: display-heading (Columns -> Void))
(define (display-heading cols)

  (: format-heading (Symbol -> String))
  (define (format-heading heading)
    (~a (symbol->string heading)
	#:width WIDTH
	#:align 'center))

  (for ([col cols])
       (let ((heading (column-heading col))
	     (series  (column-series col)))
	 (cond
           ((GenSeries? series)
            (display (format-heading heading)))
           ((NSeries? series)
            (display (format-heading heading)))
           ((CSeries? series)
            (display (format-heading heading)))
           ((ISeries? series)
            (display (format-heading heading)))
           ((BSeries? series)
            (display (format-heading heading)))
           ((DatetimeSeries? series)
            (display (format-heading heading)))
           ((DateSeries? series)
            (display (format-heading heading)))
	  (else
	   (error 'data-frame-head "Heading for unknown series types ~s"
		  (series-type series)))))
       (display " "))

  (newline))

; ***********************************************************

; ***********************************************************

(: format-gen-series (GenSeries Index -> String))
(define (format-gen-series gen-series row)
  (let ((data (gen-series-iref gen-series (list row))))
    (cond
      [(symbol? data)
          (~a (symbol->string data)
              #:width WIDTH
              #:align 'left)]
      [(integer? data)
          (~r data
              #:precision 0
              #:min-width WIDTH)]
      [(rational? data)
       (~r data
           #:precision '(= 4)
           #:min-width WIDTH)]
      [(boolean? data)
       (~a (if data
               "#t"
               "#f")
           #:width WIDTH
           #:align 'left)]
      ; pretty-format struct
      [else (pretty-format data)])))

(: format-cseries (CSeries Index -> String))
(define (format-cseries cseries row)
  (~a (symbol->string (car (cseries-iref cseries (list row))))
      #:width WIDTH
      #:align 'left))

(: format-nseries (NSeries Index -> String))
(define (format-nseries nseries row)
  (let ((n (car (nseries-iref nseries (list row)))))
    (if (rational? n)
	(~r n
	    #:precision '(= 2)
	    #:min-width WIDTH)
	"+nan.0")))

(: format-iseries (ISeries Index -> String))
(define (format-iseries iseries row)
  (let ((ref-val (car (iseries-iref iseries (list row)))))
    (if (integer? ref-val)
        (~r ref-val
            #:precision 0
            #:min-width WIDTH)
        (pretty-format ref-val))))

(: format-bseries (BSeries Index -> String))
(define (format-bseries bseries row)
  (~a (if (car (bseries-iref bseries (list row)))
          "#t"
          "#f")
      #:width WIDTH
      #:align 'left))

(: format-datetime-series (DatetimeSeries Index -> String))
(define (format-datetime-series datetime-series row)
  ;(~a (datetime->string (car (datetime-series-iref datetime-series (list row))) "~5")
      ;#:width WIDTH
      ;#:align 'left))
  (pretty-format (car (datetime-series-iref datetime-series (list row)))))

(: format-date-series (DateSeries Index -> String))
(define (format-date-series date-series row)
  (date->string (assert (car (date-series-iref date-series (list row))) date?)))

; ***********************************************************

; ***********************************************************

(: display-data-frame-row ((Vectorof Series) (Sequenceof Index) [#:delim String] -> Void))
(define (display-data-frame-row series rows #:delim [delim " "])
  ;;  (define: cols : (Sequenceof  (in-range (vector-length series)))
  (for: ([row : Index rows])
	(for ([col (in-range (vector-length series))])
	     (let ((a-series (vector-ref series col)))               
	       (cond
                 ((GenSeries? a-series)
                  (display (format-gen-series a-series row)))
                 ((NSeries? a-series)
                  (display (format-nseries a-series row)))
                 ((CSeries? a-series)
                  (display (format-cseries a-series row)))
                 ((ISeries? a-series)
                  (display (format-iseries a-series row)))
                 ((BSeries? a-series)
                  (display (format-bseries a-series row)))
                 ((DatetimeSeries? a-series)
                  (display (format-datetime-series a-series row)))
                 ((DateSeries? a-series)
                  (display (format-date-series a-series row)))
                 (else
                  (error 'display-data-frame-row "Unknown series types ~s"
                         (series-type a-series)))))
          (display delim))
        (newline)))

(define default-head-rows 10)

(: data-frame-head (case-> (DataFrame -> Void)
		      (DataFrame (Option Index) -> Void)))
(define (data-frame-head data-frame [count #f])
  (define: cols     : Columns (data-frame-explode data-frame))
  (define: headings : (Listof Label) (map column-heading cols))
  (define: series   : (Vectorof Series) (list->vector (map column-series cols)))

  ;; (show-frame-description (frame-description frame))

  (display-heading cols)

  (let ((count (min (Dim-rows (data-frame-dim data-frame))
                    (if (not count) default-head-rows count))))
    (display-data-frame-row series (in-range count))))

(: data-frame-write-delim (DataFrame [#:output-port Output-Port] [#:heading Boolean] [#:delim String] -> Void))
(define (data-frame-write-delim data-frame #:output-port [outp (current-output-port)] #:heading [heading #t] #:delim [delim "\t"])

  (define: cols     : Columns (data-frame-explode data-frame))
  (define: headings : (Listof Label) (map column-heading cols))
  (define: series   : (Vectorof Series) (list->vector (map column-series cols)))
  (define: row-num  : Index (Dim-rows (data-frame-dim data-frame)))
  (define: col-num  : Index (vector-length series))

  (: write-frame-row (Index -> Void))
  (define (write-frame-row row)
    (for ([col (in-range col-num)])
	 (unless (zero? col)
		 (display delim outp))
	 (let ((a-series (vector-ref series col)))
	   (cond
             ((GenSeries? a-series)
              (display (car (assert (gen-series-iref a-series (list row)) list?)) outp))
             ((NSeries? a-series)
              (let ((n (car (assert (nseries-iref a-series (list row)) list?))))
                (display n outp)))
             ((CSeries? a-series)
              (display (car (assert (cseries-iref a-series (list row)) list?)) outp))
             ((ISeries? a-series)
              (display (car (assert (iseries-iref a-series (list row)) list?)) outp))
             ((BSeries? a-series)
              (display (car (assert (bseries-iref a-series (list row)) list?)) outp))
             ((DatetimeSeries? a-series)
              (display (car (assert (datetime-series-iref a-series (list row)) list?)) outp))
             ((DateSeries? a-series)
              (display (car (assert (date-series-iref a-series (list row)) list?)) outp))
             (else
              (error 'frame-head "Unknown series types ~s"
                     (series-type a-series)))))))

  (: write-heading (Columns -> Void))
  (define (write-heading cols)
    (match cols
	   ((list-rest col1 cols)
	    (display (car col1) outp)
	    (for ([col cols])
		 (display delim outp)
		 (display (car col) outp))
	    (newline outp))
	   (else (void))))

  (when heading
	(write-heading cols))

  (for ([row (in-range row-num)])
       (write-frame-row (assert row index?))
       (newline outp)))

;; Write in CSV format the data frame DF to the output port OUTP.  If SERIES,
;; if non-null, denote the series to be written.  If null, all the series are
;; written out in an unspecified order.  Rows between START and STOP are
;; written out.
(: data-frame-write-csv (DataFrame Path-String -> Void))
(define (data-frame-write-csv data-frame out-file-path)
  (define out-file (open-output-file out-file-path #:exists 'truncate))
  {begin 
    (data-frame-write-delim data-frame #:output-port out-file #:heading #t #:delim ",")
    (close-output-port out-file)})

(: data-frame-write-json (DataFrame [#:output-port Output-Port] -> Void))
(define (data-frame-write-json data-frame #:output-port [outp (current-output-port)])

  (define: cols     : Columns (data-frame-explode data-frame))
  (define: headings : (Listof Label) (map column-heading cols))
  (define: series   : (Vectorof Series) (list->vector (map column-series cols)))
  (define: row-num  : Index (Dim-rows (data-frame-dim data-frame)))
  (define: col-num  : Index (vector-length series))
  (define: delim : String ",")

  (display "[" outp)

  (: write-frame-row (Index -> Void))
  (define (write-frame-row row)
    (for ([col (in-range col-num)])
      (display "\"" outp)
      (display (car (list-ref cols col)) outp)
      (display "\"" outp)
      (display ":" outp)            
      (display "\"" outp)
      (let ((a-series (vector-ref series col)))
        (cond
          ((GenSeries? a-series)
           (display (car (assert (gen-series-iref a-series (list row)) list?)) outp))
          ((NSeries? a-series)
           (let ((n (car (assert (nseries-iref a-series (list row)) list?))))
             (display n outp)))
          ((CSeries? a-series)
           (display (car (assert (cseries-iref a-series (list row)) list?)) outp))
          ((ISeries? a-series)
           (display (car (assert (iseries-iref a-series (list row)) list?)) outp))
          ((BSeries? a-series)
           (display (car (assert (bseries-iref a-series (list row)) list?)) outp))
          ((DatetimeSeries? a-series)
           (display (car (assert (datetime-series-iref a-series (list row)) list?)) outp))
          ((DateSeries? a-series)
           (display (car (assert (date-series-iref a-series (list row)) list?)) outp))
          (else
           (error 'frame-head "Unknown series types ~s"
                  (series-type a-series)))))
      (display "\"" outp)      
      (unless (eq? col (sub1 col-num))
        (display delim outp))
      ))

  (for ([row (in-range row-num)])
    (unless (zero? row)
      (display delim outp)
      (newline outp))
    (display "{" outp)
    (write-frame-row (assert row index?))
    (display "}" outp))
  
  (display "]" outp)
  (newline outp))

(: data-frame-write-json-file (DataFrame Path-String -> Void))
(define (data-frame-write-json-file data-frame out-file-path)
  (define out-file (open-output-file out-file-path #:exists 'truncate))
  {begin 
    (data-frame-write-json data-frame #:output-port out-file)
    (close-output-port out-file)})