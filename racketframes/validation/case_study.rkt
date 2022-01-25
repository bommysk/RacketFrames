#lang typed/racket

(require RacketFrames)
;(require "../main.rkt")

(define now current-inexact-milliseconds)

; has empty Footnote column
(define employment-df (load-csv-file "total_employment_by_economic_activity.csv" #:schema #f))

(println "TOTAL EMPLOYMENT DATAFRAME")
(data-frame-head employment-df)

(show-data-frame-description (data-frame-description employment-df))

(define column-filter-bench-before (now))

(define employment-df-filtered (data-frame-column-filter-not employment-df (lambda ([sex : Any]) (eq? (assert sex symbol?) '|Total men and women|)) 'Sex))

(define employment-df-filtered-men (data-frame-column-filter employment-df-filtered (lambda ([sex : Any]) (eq? (assert sex symbol?) 'Men)) 'Sex))

(define employment-df-filtered-women (data-frame-column-filter employment-df-filtered (lambda ([sex : Any]) (eq? (assert sex symbol?) 'Women)) 'Sex))

(define column-filter-bench-after (- (now) column-filter-bench-before))
(println "-------------------------")
(fprintf (current-output-port)
         "column-filter bench: ~v ms.\n"
         column-filter-bench-after)
(println "-------------------------")

; max calculations
(define data-frame-agg-max-bench-before (now))

(define max-by-country-area-df (apply-agg-data-frame 'max (data-frame-groupby (data-frame-project employment-df (list 'Country_Area 'Year 'Sex 'Value)) (list 'Country_Area 'Year 'Sex))))

(define max-by-country-area-df-subclass (apply-agg-data-frame 'max (data-frame-groupby (data-frame-project employment-df (list 'Country_Area 'Year 'Subclassification 'Value)) (list 'Country_Area 'Year 'Subclassification))))

(define max-by-country-area-df-men (apply-agg-data-frame 'max (data-frame-groupby (data-frame-project employment-df-filtered-men (list 'Country_Area 'Year 'Value)) (list 'Country_Area))))

(define max-by-country-area-df-women (apply-agg-data-frame 'max (data-frame-groupby (data-frame-project employment-df-filtered-women (list 'Country_Area 'Year 'Value)) (list 'Country_Area))))

(define data-frame-agg-max-bench-after (- (now) data-frame-agg-max-bench-before))
(println "-------------------------")
(fprintf (current-output-port)
         "data-frame-agg-max bench: ~v ms.\n"
         data-frame-agg-max-bench-after)
(println "-------------------------")

; min calculations
(define data-frame-agg-min-bench-before (now))

(define min-by-country-area-df (apply-agg-data-frame 'min (data-frame-groupby (data-frame-project employment-df (list 'Country_Area 'Year 'Sex 'Value)) (list 'Country_Area 'Year 'Sex))))

(define min-by-country-area-df-subclass (apply-agg-data-frame 'min (data-frame-groupby (data-frame-project employment-df (list 'Country_Area 'Year 'Subclassification 'Value)) (list 'Country_Area 'Year 'Subclassification))))

(define min-by-country-area-df-men (apply-agg-data-frame 'min (data-frame-groupby (data-frame-project employment-df-filtered-men (list 'Country_Area 'Year 'Value)) (list 'Country_Area))))

(define min-by-country-area-df-women (apply-agg-data-frame 'min (data-frame-groupby (data-frame-project employment-df-filtered-women (list 'Country_Area 'Year 'Value)) (list 'Country_Area))))

(define data-frame-agg-min-bench-after (- (now) data-frame-agg-min-bench-before))
(println "-------------------------")
(fprintf (current-output-port)
         "data-frame-agg-min bench: ~v ms.\n"
         data-frame-agg-min-bench-after)
(println "-------------------------")

(println "-------------------------")
(println "Max by country area men")
(println "-------------------------")
(data-frame-head max-by-country-area-df-men)

(println "-------------------------")
(println "Max by country area women")
(println "-------------------------")
(data-frame-head max-by-country-area-df-women)

(println "-------------------------")
(println "Max by country area")
(println "-------------------------")
(data-frame-head max-by-country-area-df)

(println "-------------------------")
(println "Max by country area subclass")
(println "-------------------------")
(data-frame-head max-by-country-area-df-subclass)

(println "-------------------------")
(println "Min by country area men")
(println "-------------------------")
(data-frame-head min-by-country-area-df-men)

(println "-------------------------")
(println "Min by country area women")
(println "-------------------------")
(data-frame-head min-by-country-area-df-women)

(println "-------------------------")
(println "Min by country area")
(println "-------------------------")
(data-frame-head min-by-country-area-df)

(println "-------------------------")
(println "Min by country area subclass")
(println "-------------------------")
(data-frame-head min-by-country-area-df-subclass)

(define series-sort-descending-bench-before (now))

(println "-------------------------")
(println "Max by country area series")
(println "-------------------------")
(define max-by-country-area-series : Series (series-sort-descending (data-frame-series-ref max-by-country-area-df 'Value)))
(series-print max-by-country-area-series #:count 10)

(println "-------------------------")
(println "Max by country area men series")
(println "-------------------------")
(define max-by-country-area-men-series : Series (series-sort-descending (data-frame-series-ref max-by-country-area-df-men 'Value)))
(series-print max-by-country-area-men-series #:count 10)

(println "-------------------------")
(println "Max by country area women series")
(println "-------------------------")
(series-print (series-sort-descending (data-frame-series-ref max-by-country-area-df-women 'Value)) #:count 10)

(println "-------------------------")
(println "Max by country area subclass series")
(println "-------------------------")
(define max-by-country-area-subclass-series : Series (series-sort-descending (data-frame-series-ref max-by-country-area-df-subclass 'Value)))
(series-print max-by-country-area-subclass-series #:count 10)

(define series-sort-descending-bench-after (- (now) series-sort-descending-bench-before))
(println "-------------------------")
(fprintf (current-output-port)
         "series-sort-descending bench: ~v ms.\n"
         series-sort-descending-bench-after)
(println "-------------------------")

(define series-sort-bench-before (now))
(println "-------------------------")
(println "Min by country area series")
(println "-------------------------")
(series-print (series-sort (data-frame-series-ref max-by-country-area-df 'Value)) #:count 10)

(println "-------------------------")
(println "Min by country area men series")
(println "-------------------------")
(series-print (series-sort (data-frame-series-ref max-by-country-area-df-men 'Value)) #:count 10)

(println "-------------------------")
(println "Min by country area women series")
(println "-------------------------")
(series-print (series-sort (data-frame-series-ref max-by-country-area-df-women 'Value)) #:count 10)

(println "-------------------------")
(println "Min by country area subclass series")
(println "-------------------------")
(series-print (series-sort (data-frame-series-ref max-by-country-area-df-subclass 'Value)) #:count 10)
(define series-sort-bench-after (- (now) series-sort-bench-before))
(println "-------------------------")
(fprintf (current-output-port)
         "series-sort bench: ~v ms.\n"
         series-sort-bench-after)
(println "-------------------------")

(data-frame-head max-by-country-area-df)

(show-data-frame-description (data-frame-description max-by-country-area-df))

(series-loc (data-frame-series-ref max-by-country-area-df-men 'Value) 'China)

(define data-frame-agg-count-bench-before (now))
(println "-------------------------")
(println "Count by source series for men")
(println "-------------------------")

(define count-by-source-df-men (apply-agg-data-frame 'count (data-frame-groupby employment-df-filtered-men (list 'Source))))

(data-frame-head count-by-source-df-men)

(show-data-frame-description (data-frame-description count-by-source-df-men))

(series-print (data-frame-series-ref count-by-source-df-men 'Value))

(println "-------------------------")
(println "Count by country source series for men")
(println "-------------------------")
(define count-by-country-source-df-men (apply-agg-data-frame 'count (data-frame-groupby employment-df-filtered-men (list 'Country_Area 'Source))))

(data-frame-head count-by-country-source-df-men)

(series-print (series-sort-descending (data-frame-series-ref count-by-country-source-df-men 'Value)) #:count 10)

(println "-------------------------")
(println "Count by source series women")
(println "-------------------------")

(define count-by-source-df-women (apply-agg-data-frame 'count (data-frame-groupby employment-df-filtered-women (list 'Source))))

(data-frame-head count-by-source-df-women)

(series-print (series-sort-descending (data-frame-series-ref count-by-source-df-women 'Value)))

(println "-------------------------")
(println "Count by country source series women")
(println "-------------------------")

(define count-by-country-source-df-women (apply-agg-data-frame 'count (data-frame-groupby employment-df-filtered-women (list 'Country_Area 'Source))))

(data-frame-head count-by-country-source-df-women)

(series-print (series-sort-descending (data-frame-series-ref count-by-country-source-df-women 'Value)) #:count 10)

(define data-frame-agg-count-bench-after (- (now) data-frame-agg-count-bench-before))
(println "-------------------------")
(fprintf (current-output-port)
         "data-frame-agg-count bench: ~v ms.\n"
         data-frame-agg-count-bench-after)
(println "-------------------------")