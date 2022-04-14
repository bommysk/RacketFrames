#lang typed/racket

; ***********************************************************
; use build-index-from-labels function and Label, SIndex and
; LabelIndex structs from indexed-series.
(require RacketFrames)

(require racket/format
         racket/fixnum)
; ***********************************************************

 #| class Plotting(object):

    goal_time = 0.2

    def setup(self):
        self.s = Series(np.random.randn(1000000))
        self.df = DataFrame({'col': self.s})

    def time_series_plot(self):
        self.s.plot()

    def time_frame_plot(self):
        self.df.plot()

class TimeseriesPlotting(object):

    goal_time = 0.2

    def setup(self):
        N = 2000
        M = 5
        idx = date_range('1/1/1975', periods=N)
        self.df = DataFrame(np.random.randn(N, M), index=idx)

        idx_irregular = DatetimeIndex(np.concatenate((idx.values[0:10],
                                                      idx.values[12:])))
        self.df2 = DataFrame(np.random.randn(len(idx_irregular), M),
                             index=idx_irregular)

    def time_plot_regular(self):
        self.df.plot()

    def time_plot_regular_compat(self):
        self.df.plot(x_compat=True)

    def time_plot_irregular(self):
        self.df2.plot() |#

; 1000000 elements for scatter and line plot
(: N Positive-Integer)
(define N 1000)
(: N-MIN Negative-Integer)
(define N-MIN (* -1 N))

(: data (Vectorof Fixnum))
(define data (for/vector: : (Vectorof Fixnum)
    ([n : Real (range N)])
  (random (add1 (assert n fixnum?)))))

(define integer-series (new-ISeries data))

(define columns-integer (list (cons 'col integer-series)))

(define data-frame-integer (new-data-frame columns-integer))

; 10000 elements for histogram
(: HISTOGRAM-N Positive-Integer)
(define HISTOGRAM-N 50)
(: HISTOGRAM-N-MIN Negative-Integer)
(define HISTOGRAM-N-MIN (* -1 HISTOGRAM-N))

(: histogram-data (Vectorof Fixnum))
(define histogram-data (for/vector: : (Vectorof Fixnum)
    ([n : Real (range HISTOGRAM-N)])
  (random (add1 (assert n fixnum?)))))

(define histogram-integer-series (new-ISeries histogram-data))

(define histogram-columns-integer (list (cons 'col histogram-integer-series)))

(define plot-columns-integer (list (cons 'x integer-series) (cons 'y integer-series)))

(define plot-data-frame-integer (new-data-frame plot-columns-integer))

(define histogram-data-frame-integer (new-data-frame histogram-columns-integer))

; benchmark start
(define now current-inexact-milliseconds)

(define integer-series-scatter-plot-bench-before (now))
(make-scatter-plot integer-series #:x-min N-MIN #:x-max N #:y-min N-MIN #:y-max N)
(define integer-series-scatter-plot-bench-after (- (now) integer-series-scatter-plot-bench-before))

(fprintf (current-output-port)
         "Integer Series Scatter Plot Bench: ~v ms.\n"
         integer-series-scatter-plot-bench-after)

(define integer-dataframe-scatter-plot-bench-before (now))
(make-scatter-plot plot-data-frame-integer)
(define integer-dataframe-scatter-plot-bench-after (- (now) integer-dataframe-scatter-plot-bench-before))

(fprintf (current-output-port)
         "Integer Dataframe Scatter Plot Bench: ~v ms.\n"
         integer-dataframe-scatter-plot-bench-after)

(define integer-series-line-plot-bench-before (now))
(make-line-plot integer-series #:x-min N-MIN #:x-max N #:y-min N-MIN #:y-max N)
(define integer-series-line-plot-bench-after (- (now) integer-series-line-plot-bench-before))

(fprintf (current-output-port)
         "Integer Series Line Plot Bench: ~v ms.\n"
         integer-dataframe-scatter-plot-bench-after)

(define integer-dataframe-line-plot-bench-before (now))
(make-line-plot plot-data-frame-integer)
(define integer-dataframe-line-plot-bench-after (- (now) integer-dataframe-line-plot-bench-before))

(fprintf (current-output-port)
         "Integer Dataframe Line Plot Bench: ~v ms.\n"
         integer-dataframe-line-plot-bench-after)

(define integer-series-discrete-historgram-bench-before (now))
(make-discrete-histogram integer-series)
(define integer-series-discrete-historgram-bench-after (- (now) integer-series-discrete-historgram-bench-before))

(fprintf (current-output-port)
         "Integer Series Discrete Histogram Bench: ~v ms.\n"
         integer-series-discrete-historgram-bench-after)

(define integer-dataframe-discrete-histogram-bench-before (now))
(make-discrete-histogram histogram-data-frame-integer)
(define integer-dataframe-discrete-histogram-bench-after (- (now) integer-dataframe-discrete-histogram-bench-before))

(fprintf (current-output-port)
         "Integer Dataframe Discrete Histogram Bench: ~v ms.\n"
         integer-dataframe-discrete-histogram-bench-after)


(define integer-series-discrete-historgram-stacked-bench-before (now))
(make-discrete-histogram-stacked integer-series)
(define integer-series-discrete-historgram-stacked-bench-after (- (now) integer-series-discrete-historgram-stacked-bench-before))

(fprintf (current-output-port)
         "Integer Series Discrete Histogram Stacked Bench: ~v ms.\n"
         integer-series-discrete-historgram-stacked-bench-after)

(define integer-dataframe-discrete-histogram-stacked-bench-before (now))
(make-discrete-histogram-stacked histogram-data-frame-integer)
(define integer-dataframe-discrete-histogram-stacked-bench-after (- (now) integer-dataframe-discrete-histogram-stacked-bench-before))

(fprintf (current-output-port)
         "Integer Dataframe Discrete Histogram Stacked Bench: ~v ms.\n"
         integer-dataframe-discrete-histogram-stacked-bench-after)


; datetime range generation
(define date-range-bench-before (now))
;(build-index-from-list (datetime-range (Datetime (Date 1975 1 1) (Time 0 0 0 0 0)) 'D 2000 #f))
(define date-range-bench-after (- (now) date-range-bench-before))

(fprintf (current-output-port)
         "Date Range Generation Bench: ~v ms.\n"
         date-range-bench-after)