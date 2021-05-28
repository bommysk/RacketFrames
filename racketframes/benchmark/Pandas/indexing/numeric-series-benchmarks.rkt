#lang typed/racket

; ***********************************************************
; use build-index-from-labels function and Label, SIndex and
; LabelIndex structs from indexed-series.
(require RacketFrames)

(require racket/format
         racket/flonum)

; ***********************************************************

 #| def setup(self, index):
        N = 10**6
        idx = index(range(N))
        self.data = Series(np.random.rand(N), index=idx)
        self.array = np.arange(10000)
        self.array_list = self.array.tolist()

    def time_getitem_scalar(self, index):
        self.data[800000]

    def time_getitem_slice(self, index):
        self.data[:800000]

    def time_getitem_list_like(self, index):
        self.data[[800000]]

    def time_getitem_array(self, index):
        self.data[self.array]

    def time_getitem_lists(self, index):
        self.data[self.array_list]

    def time_iloc_array(self, index):
        self.data.iloc[self.array]

    def time_iloc_list_like(self, index):
        self.data.iloc[[800000]]

    def time_iloc_scalar(self, index):
        self.data.iloc[800000]

    def time_iloc_slice(self, index):
        self.data.iloc[:800000]

    def time_ix_array(self, index):
        self.data.ix[self.array]

    def time_ix_list_like(self, index):
        self.data.ix[[800000]]

    def time_ix_scalar(self, index):
        self.data.ix[800000]

    def time_ix_slice(self, index):
        self.data.ix[:800000]

    def time_loc_array(self, index):
        self.data.loc[self.array]

    def time_loc_list_like(self, index):
        self.data.loc[[800000]]

    def time_loc_scalar(self, index):
        self.data.loc[800000]

    def time_loc_slice(self, index):
        self.data.loc[:800000] |#

(define now current-inexact-milliseconds)

(define N (expt 10 6))

(: data FlVector)
(define data (make-flvector N (real->double-flonum (random N))))

(define series-float (new-NSeries data))

#| def time_getitem_scalar(self, index):
      self.data[800000] |#
(define i-loc-bench-before (now))
(define nseries-iloc-value (nseries-iloc series-float 80000))
(define i-loc-bench-after (- (now) i-loc-bench-before))

(fprintf (current-output-port)
         "Numeric Series i-ref bench ~v ms.\n"
         i-loc-bench-after)

(printf "Pandas Compare* indexing.NumericSeriesIndexing.time_getitem_scalar 25.58μs \n")

#| def time_getitem_slice(self, index):
        self.data[:800000] |#
(define n-range-bench-before (now))
(define nseries-range-80000 (nseries-range series-float 80000))
(define n-range-bench-after (- (now) n-range-bench-before))

(fprintf (current-output-port)
         "Numeric Series range bench ~v ms.\n"
         n-range-bench-after)

(printf "Pandas Compare* indexing.NumericSeriesIndexing.time_getitem_slice: 103.22μs. \n")


(: label-index (Listof Symbol))
(define label-index (for/list: : (Listof Symbol)
                      ([i (range N)])
                      (string->symbol (string-append "a" (number->string i)))))

(define series-float-with-label-index (new-NSeries data
                                                     #:index (build-index-from-list label-index)))

(define n-ref-label-bench-before (now))
(define nseries-label-ref-value (nseries-loc series-float-with-label-index 'a100))
(define n-ref-label-bench-after (- (now) n-ref-label-bench-before))

(fprintf (current-output-port)
         "Numeric Series ref label bench ~v ms.\n"
         n-ref-label-bench-after)

(printf "Pandas Compare* NumericSeriesIndexing.time_ix_scalar 80.14μs. \n")

(define n-ref-label-list-bench-before (now))
(define nseries-label-list-ref-value (nseries-loc series-float-with-label-index (list 'a100)))
(define n-ref-label-list-bench-after (- (now) n-ref-label-bench-before))

(fprintf (current-output-port)
         "Numeric Series ref label list bench ~v ms.\n"
         n-ref-label-list-bench-after)

(printf "Pandas Compare* NumericSeriesIndexing.time_ix_scalar 80.14μs. \n")