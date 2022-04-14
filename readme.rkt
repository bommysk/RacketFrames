#lang typed/racket

(require RacketFrames)

;******************
;data-frame-mix
;******************
(define columns-mix
  (list
   (cons 'integer-col (new-ISeries (list 1 2 3 4)
                            #:index (list 'a 'b 'c 'd)))
   (cons 'categorical-col (new-CSeries (list 'hello 'world 'fizz 'buzz)))))

; Let RacketFrames determine the Series type
(define columns-mix-2
  (list
   (cons 'integer-col (new-series (list 1 2 3 4)
                            #:index (list 'a 'b 'c 'd)))
   (cons 'categorical-col (new-series (list 'hello 'world 'fizz 'buzz)))))

; create new data-frame-mix
(define data-frame-mix (new-data-frame columns-mix))

(data-frame-write-delim data-frame-mix)

(show-data-frame-description (data-frame-description data-frame-mix))

; create new data-frame-mix-2
(define data-frame-mix-2 (new-data-frame columns-mix))

(data-frame-write-delim data-frame-mix-2)

(data-frame-write-delim data-frame-mix)

(show-data-frame-description (data-frame-description data-frame-mix-2))

(define salary-data-frame-csv-no-schema (load-csv-file "racketframes/sample-csv/salary_date.csv"))

(data-frame-head salary-data-frame-csv-no-schema)

(displayln "DataFrame List of Column Names")
(data-frame-names salary-data-frame-csv-no-schema)
(show-data-frame-description (data-frame-description salary-data-frame-csv-no-schema))

(build-multi-index-from-list (list (list 'a 'b 'c 'd 'e) (list 1 2 3 4 5) (list 'aa 'bb 'cc 'dd 'ee)))