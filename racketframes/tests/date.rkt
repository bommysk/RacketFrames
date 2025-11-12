#lang typed/racket
(require typed/rackunit)

(require "../data-frame/date.rkt")
(require "../data-frame/series-description.rkt")
(require "../data-frame/categorical-series.rkt")
(require "../data-frame/integer-series.rkt")

(define cseries (new-CSeries (vector '2005-12-12 '2019-12-25) #f))

(check-equal? (series-iloc (as-julian-day-numbers cseries) 0) 2453717)

(check-equal? (series-iloc (as-julian-day-numbers cseries) 1) 2458843)

