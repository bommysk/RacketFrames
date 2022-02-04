#lang typed/racket

; ***********************************************************
; use build-index-from-labels function and Label, SIndex and
; LabelIndex structs from indexed-series.
(require RacketFrames)

(require racket/format
         racket/flonum)

; ***********************************************************

(define now current-inexact-milliseconds)

(define N (expt 10 6))

(: data FlVector)
(define data (make-flvector N (real->double-flonum (random N))))

(define series-float (new-NSeries data))

#| def time_getitem_scalar(self, index):
      self.data[800000] |#
(define nseries-sort-bench-before (now))
(define nseries-sort-result (nseries-sort series-float))
(define nseries-sort-bench-after (- (now) nseries-sort-bench-before))

(fprintf (current-output-port)
         "Numeric Series Sort Bench: ~v ms.\n"
         nseries-sort-bench-after)

(define nseries-sort-descending-bench-before (now))
(define nseries-sort-descending-result (nseries-sort-descending series-float))
(define nseries-sort-descending-bench-after (- (now) nseries-sort-descending-bench-before))

(fprintf (current-output-port)
         "Numeric Series Sort Descending Bench: ~v ms.\n"
         nseries-sort-descending-bench-after)