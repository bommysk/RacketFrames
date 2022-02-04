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
(define nseries-greater-than-filter-bench-before (now))
(define nseries-greater-than-filter (nseries-filter series-float (lambda ([x : Flonum]) (> (random N) x))))
(define nseries-greater-than-filter-bench-after (- (now) nseries-greater-than-filter-bench-before))

(fprintf (current-output-port)
         "Integer Series greater-than? filter Bench: ~v ms.\n"
         nseries-greater-than-filter-bench-after)

#| def time_getitem_slice(self, index):
        self.data[:800000] |#
(define nseries-less-than-filter-bench-before (now))
(define nseries-less-than-filter (nseries-filter series-float (lambda ([x : Flonum]) (< (random N) x))))
(define nseries-less-than-filter-bench-after (- (now) nseries-less-than-filter-bench-before))

(fprintf (current-output-port)
         "Integer Series less-than? filter ~v ms.\n"
         nseries-less-than-filter-bench-after)
