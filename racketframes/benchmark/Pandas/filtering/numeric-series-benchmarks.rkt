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

#| import random as random
data = np.random.random(1000000)
s = pd.Series(data)
s.loc[lambda x: random.random() > x]
CPU times: user 2 μs, sys: 1 μs, total: 3 μs
Wall time: 5.01 μs
|#
(define nseries-greater-than-filter-bench-before (now))
(define nseries-greater-than-filter (nseries-filter series-float (lambda ([x : Flonum]) (> (random N) x))))
(define nseries-greater-than-filter-bench-after (- (now) nseries-greater-than-filter-bench-before))

(fprintf (current-output-port)
         "Integer Series greater-than? filter Bench: ~v ms.\n"
         nseries-greater-than-filter-bench-after)

#| data = np.random.random(1000000)
s = pd.Series(data)
s.loc[lambda x: random.random() < x]
 CPU times: user 3 μs, sys: 1 μs, total: 4 μs
Wall time: 3.81 μs|#
(define nseries-less-than-filter-bench-before (now))
(define nseries-less-than-filter (nseries-filter series-float (lambda ([x : Flonum]) (< (random N) x))))
(define nseries-less-than-filter-bench-after (- (now) nseries-less-than-filter-bench-before))

(fprintf (current-output-port)
         "Integer Series less-than? filter ~v ms.\n"
         nseries-less-than-filter-bench-after)
