#lang setup/infotab

(define name "RacketFrames")

(define scribblings '(("documentation/dataframe.scrbl" ())))

(define compile-omit-paths '("tests"                            
                             "benchmark"                             
                             "validation"
                             "jupyter-notebooks"
                             "sample-csv"
                             "util/future-work"
                             "stats/future-work"))