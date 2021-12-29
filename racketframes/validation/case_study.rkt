#lang typed/racket

(require "../main.rkt")

; has empty Footnote column
(define employment-df (load-csv-file "total_employment_by_economic_activity.csv" #:schema #f))

(println "TOTAL EMPLOYMENT DATAFRAME")
(data-frame-head employment-df)

(show-data-frame-description (data-frame-description employment-df))

(define employment-df-filtered (data-frame-column-filter-not employment-df (lambda ([sex : Any]) (eq? (assert sex symbol?) '|Total men and women|)) 'Sex))

(define employment-df-filtered-men (data-frame-column-filter employment-df-filtered (lambda ([sex : Any]) (eq? (assert sex symbol?) 'Men)) 'Sex))
(define employment-df-filtered-women (data-frame-column-filter employment-df-filtered (lambda ([sex : Any]) (eq? (assert sex symbol?) 'Women)) 'Sex))

(define max-by-country-area-df (apply-agg-data-frame 'max (data-frame-groupby (data-frame-project employment-df-filtered (list 'Country_Area 'Year 'Sex 'Value)) (list 'Country_Area 'Sex))))

(define max-by-country-area-df-men (apply-agg-data-frame 'max (data-frame-groupby (data-frame-project employment-df-filtered-men (list 'Country_Area 'Year 'Value)) (list 'Country_Area))))

(define max-by-country-area-df-women (apply-agg-data-frame 'max (data-frame-groupby (data-frame-project employment-df-filtered-women (list 'Country_Area 'Year 'Value)) (list 'Country_Area))))

;(sort (vector->list (series-data (data-frame-series-ref max-by-country-area-df-men 'Value))) <)
;(sort (vector->list (series-data (data-frame-series-ref max-by-country-area-df-women 'Value))) <)

(data-frame-head max-by-country-area-df-men)
(data-frame-head max-by-country-area-df-women)

(data-frame-head max-by-country-area-df)

;(series-filter (series-data (data-frame-series-ref max-by-country-area-df 'Value)))

(show-data-frame-description (data-frame-description max-by-country-area-df))

(data-frame-index employment-df-filtered)

(series-print (assert (data-frame-loc employment-df-filtered 'Jamaica (list 'Value 'Year)) Series?))

(println "HIGHEST VALUE FOR JAMAICA")
(series-loc (assert (data-frame-loc employment-df-filtered 'Jamaica (list 'Value 'Year)) Series?) 'Value)
