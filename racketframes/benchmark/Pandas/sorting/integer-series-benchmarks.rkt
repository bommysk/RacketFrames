#lang typed/racket

; ***********************************************************
; use build-index-from-labels function and Label, SIndex and
; LabelIndex structs from indexed-series.
(require RacketFrames)

(require racket/format)

(define now current-inexact-milliseconds)

(define N (expt 10 6))

(: data (Vectorof Fixnum))
(define data (make-vector N (random N)))

(define series-integer (new-ISeries data))

#| def time_getitem_scalar(self, index):
      self.data[800000] |#
(define iseries-sort-bench-before (now))
(define iseries-sort-value (iseries-sort series-integer))
(define iseries-sort-bench-after (- (now) iseries-sort-bench-before))

(fprintf (current-output-port)
         "Integer Series Sort Bench: ~v ms.\n"
         iseries-sort-bench-after)

(define iseries-sort-descending-bench-before (now))
(define iseries-sort-descending-result (iseries-sort-descending series-integer))
(define iseries-sort-descending-bench-after (- (now) iseries-sort-descending-bench-before))

(fprintf (current-output-port)
         "Integer Series Sort Descending Bench: ~v ms.\n"
         iseries-sort-descending-bench-after)
