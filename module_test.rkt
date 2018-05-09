#lang typed/racket

(require RacketFrames)

;******************
;data-frame-mix
;******************
(define columns-mix
  (list
   (cons 'integer-col (new-ISeries (vector 1 2 3 4)
                            (build-index-from-labels (list 'a 'b 'c 'd))))
   (cons 'categorical-col (new-CSeries (vector 'hello 'world 'fizz 'buzz)))))

; create new data-frame-mix
(define data-frame-mix (new-data-frame columns-mix))

(frame-write-tab data-frame-mix (current-output-port))

; no schema
(define demographics-data-frame-csv-no-schema (load-csv-file (FilePath "sample-csv/Demographic_Statistics_By_Zip_Code.csv") #:schema #f #:delim ","))

(frame-write-tab fruits-data-frame-csv-no-schema (current-output-port))