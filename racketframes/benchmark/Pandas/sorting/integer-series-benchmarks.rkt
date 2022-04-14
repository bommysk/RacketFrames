#lang typed/racket

; ***********************************************************
; use build-index-from-labels function and Label, SIndex and
; LabelIndex structs from indexed-series.
(require RacketFrames)

(require racket/format)

(require "../../../util/list.rkt")

(define now current-inexact-milliseconds)

(define N (expt 10 4))

(: data (Listof Fixnum))
(define data (assert (random-number-list (assert N index?) (assert N index?)) ListofFixnum?))

(define series-integer (new-ISeries data))

#| data = np.random.randint(0,100000,size=1000000)
pd.Series(data).sort_values()
 CPU times: user 2 μs, sys: 0 ns, total: 2 μs
Wall time: 5.01 μs|#
(define iseries-sort-bench-before (now))
(define iseries-sort-value (iseries-sort series-integer))
(define iseries-sort-bench-after (- (now) iseries-sort-bench-before))

(fprintf (current-output-port)
         "Integer Series Sort Bench: ~v ms.\n"
         iseries-sort-bench-after)

#| data = np.random.randint(0,100000,size=1000000)
pd.Series(data).sort_values(ascending=False)
CPU times: user 2 μs, sys: 0 ns, total: 2 μs
Wall time: 4.29 μs |#
(define iseries-sort-descending-bench-before (now))
(define iseries-sort-descending-result (iseries-sort-descending series-integer))
(define iseries-sort-descending-bench-after (- (now) iseries-sort-descending-bench-before))

(fprintf (current-output-port)
         "Integer Series Sort Descending Bench: ~v ms.\n"
         iseries-sort-descending-bench-after)

(define iseries-unique-bench-before (now))
(define iseries-unique-result (iseries-unique series-integer))
(define iseries-unique-bench-after (- (now) iseries-sort-descending-bench-before))

(fprintf (current-output-port)
         "Integer Series Unique Bench: ~v ms.\n"
         iseries-unique-bench-after)

(define iseries-isna-bench-before (now))
(define iseries-isna-result (iseries-isna series-integer))
(define iseries-isna-bench-after (- (now) iseries-sort-descending-bench-before))

(fprintf (current-output-port)
         "Integer Series isna Bench: ~v ms.\n"
         iseries-isna-bench-after)

(define iseries-notna-bench-before (now))
(define iseries-notna-result (iseries-isna series-integer))
(define iseries-notna-bench-after (- (now) iseries-sort-descending-bench-before))

(fprintf (current-output-port)
         "Integer Series notna Bench: ~v ms.\n"
         iseries-notna-bench-after)