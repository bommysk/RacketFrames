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

(iseries-head series-integer)

#| data = np.random.randint(0,100000,size=1000000)
s = pd.Series(data)
s.loc[lambda x: x % 2 == 0]
CPU times: user 2 μs, sys: 1 μs, total: 3 μs
Wall time: 5.01 μs|#
(define iseries-even-filter-bench-before (now))
(define iseries-even-filter (iseries-filter series-integer (lambda ([x : RFFixnum]) (even? (assert x integer?)))))
(define iseries-even-filter-bench-after (- (now) iseries-even-filter-bench-before))

(fprintf (current-output-port)
         "Integer Series even? filter Bench: ~v ms.\n"
         iseries-even-filter-bench-after)

#| data = np.random.randint(0,100000,size=1000000)
s = pd.Series(data)
s.loc[lambda x: x % 2 != 0]
CPU times: user 2 μs, sys: 1 μs, total: 3 μs
Wall time: 5.01 μs
|#
(define iseries-odd-filter-bench-before (now))
(define iseries-odd-filter (iseries-filter series-integer (lambda ([x : RFFixnum]) (odd? (assert x integer?)))))
(define iseries-odd-filter-bench-after (- (now) iseries-odd-filter-bench-before))

(fprintf (current-output-port)
         "Integer Series odd? filter ~v ms.\n"
         iseries-odd-filter-bench-after)
