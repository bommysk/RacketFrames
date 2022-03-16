#lang typed/racket

(require
 racket/pretty
 racket/unsafe/ops
 racket/flonum
 racket/set
 (only-in racket/set
	  set set-member?
	  list->set set->list
	  set-intersect set-subtract))

(require RacketFrames)


#|  def setup(self, axis, ignore_index):
        frame_c = DataFrame(np.zeros((10000, 200),
                            dtype=np.float32, order='C'))
        self.frame_c = [frame_c] * 20
        frame_f = DataFrame(np.zeros((10000, 200),
                            dtype=np.float32, order='F'))
        self.frame_f = [frame_f] * 20

    def time_c_ordered(self, axis, ignore_index):
        concat(self.frame_c, axis=axis, ignore_index=ignore_index)

    def time_f_ordered(self, axis, ignore_index):
        concat(self.frame_f, axis=axis, ignore_index=ignore_index)
|#

(define now current-inexact-milliseconds)

; Setup
(define N (expt 10 4))

(define frame-c (new-data-frame
                 (for/list: : Columns ([col 200])
                   (cons (string->symbol (number->string (+ col (random))))
                           (new-ISeries (for/vector: : (Vectorof Fixnum) ([i N]) 0))))))

(define twenty-frame-c (for/list: : (Listof DataFrame) ([i 20]) (new-data-frame
                 (for/list: : Columns ([col 200])
                   (cons (string->symbol (~a "df~" i "~" col))
                           (new-ISeries (for/vector: : (Vectorof Fixnum) ([i N]) 0)))))))

(define frame-f (new-data-frame
                 (for/list: : Columns ([col 200])
                   (cons (string->symbol (~a "df~" col))
                           (new-ISeries (for/vector: : (Vectorof Fixnum) ([i N]) 0))))))

(define twenty-frame-f (for/list: : (Listof DataFrame) ([i 20]) frame-f))

; horizontal concat benchmark
(define data-frame-list-horizontal-concat-bench-before (now))
(define concat-horizontal-result (data-frame-concat-horizontal-list twenty-frame-c))
(define data-frame-list-horizontal-concat-bench-after (- (now) data-frame-list-horizontal-concat-bench-before))

(show-data-frame-description (data-frame-description concat-horizontal-result))

;(data-frame-head concat-horizontal-result)

(fprintf (current-output-port)
         "DataFrame list horizontal concat bench ~v ms.\n"
         data-frame-list-horizontal-concat-bench-after)
(printf "Pandas Compare* join_merge.ConcatDataFrames.time_c_ordered 123.69ms")

;vertical concat benchmark
(define data-frame-list-vertical-concat-bench-before (now))
(define concat-vertical-result (data-frame-concat-vertical-list twenty-frame-f))
(define data-frame-list-vertical-concat-bench-after (- (now) data-frame-list-vertical-concat-bench-before))

(show-data-frame-description (data-frame-description concat-vertical-result))

(fprintf (current-output-port)
         "DataFrame list vertical concat bench ~v ms.\n"
         data-frame-list-vertical-concat-bench-after)
(printf "Pandas Compare* join_merge.ConcatDataFrames.time_f_ordered 123.28ms")


(define iseries-append-bench-before (now))
(define iseries-appended (iseries-append (new-ISeries (for/vector: : (Vectorof Fixnum) ([i N]) 0)) (new-ISeries (for/vector: : (Vectorof Fixnum) ([i N]) 0))))
(define iseries-append-bench-after (- (now) iseries-append-bench-before))
;(series-print iseries-appended)
(fprintf (current-output-port)
         "iseries append bench ~v ms.\n"
         iseries-append-bench-after)

(define cseries-append-bench-before (now))
(define cseries-appended (cseries-append (new-CSeries (for/vector: : (Vectorof Symbol) ([i N]) 'a)) (new-CSeries (for/vector: : (Vectorof Symbol) ([i N]) 'b))))
(define cseries-append-bench-after (- (now) cseries-append-bench-before))
;(series-print cseries-appended)
(fprintf (current-output-port)
         "cseries append bench ~v ms.\n"
         cseries-append-bench-after)