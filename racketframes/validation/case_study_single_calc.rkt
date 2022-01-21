#lang typed/racket

(require RacketFrames)

; has empty Footnote column
(define employment-df (load-csv-file "total_employment_by_economic_activity.csv" #:schema #f))



(println "-------------------------")
(println "Count by source series")
(println "-------------------------")

(define count-by-source-df (apply-agg-data-frame 'mode (data-frame-groupby employment-df (list 'Source))))

;(data-frame-head count-by-source-df)

;(series-print (series-sort-descending (data-frame-series-ref count-by-source-df 'Value)))

(println "-------------------------")
(println "Count by country source series")
(println "-------------------------")

(define count-by-country-source-df (apply-agg-data-frame 'mode (data-frame-groupby employment-df (list 'Country_Area 'Source))))

;(data-frame-head count-by-country-source-df)

;(series-print (series-sort-descending (data-frame-series-ref count-by-country-source-df 'Value)) #:count 10)