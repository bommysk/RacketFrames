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
(define iseries-even-filter-bench-before (now))
(define iseries-even-filter (iseries-filter series-integer (lambda ([x : RFFixnum]) (even? (assert x integer?)))))
(define iseries-even-filter-bench-after (- (now) iseries-even-filter-bench-before))

(fprintf (current-output-port)
         "Integer Series even? filter Bench: ~v ms.\n"
         iseries-even-filter-bench-after)

#| def time_getitem_slice(self, index):
        self.data[:800000] |#
(define iseries-odd-filter-bench-before (now))
(define iseries-odd-filter (iseries-filter series-integer (lambda ([x : RFFixnum]) (odd? (assert x integer?)))))
(define iseries-odd-filter-bench-after (- (now) iseries-odd-filter-bench-before))

(fprintf (current-output-port)
         "Integer Series odd? filter ~v ms.\n"
         iseries-odd-filter-bench-after)
