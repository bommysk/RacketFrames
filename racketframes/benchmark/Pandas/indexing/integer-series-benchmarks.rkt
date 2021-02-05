#lang typed/racket

; ***********************************************************
; use build-index-from-labels function and Label, SIndex and
; LabelIndex structs from indexed-series.
(require RacketFrames)

(require racket/format)
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

(: data (Vectorof Fixnum))
(define data (make-vector N (random N)))

(define series-integer (new-ISeries data #f))

#| def time_getitem_scalar(self, index):
      self.data[800000] |#
(define i-ref-bench-before (now))
(define iseries-iref-value (iseries-iloc series-integer 80000))
(define i-ref-bench-after (- (now) i-ref-bench-before))

(iseries-iloc series-integer 80000)

(fprintf (current-output-port)
         "Integer Series i-loc Bench: ~v ms.\n"
         i-ref-bench-after)

#| def time_getitem_slice(self, index):
        self.data[:800000] |#
(define i-range-bench-before (now))
(define iseries-range-80000 (iseries-range series-integer 0 80000))
(define i-range-bench-after (- (now) i-range-bench-before))

(fprintf (current-output-port)
         "Integer Series range bench ~v ms.\n"
         i-range-bench-after)

#| def time_getitem_list_like(self, index):
        self.data[[800000]] |#
(define i-list-like-bench-before (now))
(define iseries-list-like-80000 (iseries-iloc series-integer (list 80000)))
(define i-list-like-bench-after (- (now) i-list-like-bench-before))

(fprintf (current-output-port)
         "Integer Series list-like bench ~v ms.\n"
         i-list-like-bench-after)

#| def time_iloc_slice(self, index):
        self.data.iloc[:800000] |#

(: iseries-iloc-range (ISeries Index Index -> ISeries))
(define (iseries-iloc-range iseries start end)
  ; use vector-copy library method
  (new-ISeries
   (vector-copy (iseries-data iseries) start end)
   (if (not (ISeries-index iseries))
       #f
       (build-index-from-list (map (lambda ([i : Index]) (idx->key (assert (ISeries-index iseries)) i)) (range start end))))))

(define i-iloc-range-80000-before (now))
;(define iseries-iloc-range-80000 (iseries-iloc series-integer (range 80000)))
(iseries-iloc-range series-integer 0 80000)
(define i-iloc-range-80000-after (- (now) i-list-like-bench-before))

(fprintf (current-output-port)
         "Integer Series iloc range bench ~v ms.\n"
         i-iloc-range-80000-after)

(: label-index (Listof Symbol))
(define label-index (for/list: : (Listof Symbol)
                      ([i N])
                      (string->symbol (string-append "a" (number->string i)))))

(define series-integer-with-label-index (new-ISeries data
                                                     (build-index-from-list label-index)))

(define i-ref-label-bench-before (now))
(define iseries-label-ref-value (iseries-loc series-integer-with-label-index 'a80000))
(define i-ref-label-bench-after (- (now) i-ref-label-bench-before))

(fprintf (current-output-port)
         "Integer Series loc bench: ~v ms.\n"
         i-ref-label-bench-after)
