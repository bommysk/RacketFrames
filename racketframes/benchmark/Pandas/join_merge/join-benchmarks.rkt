#lang typed/racket

(require RacketFrames)

(require "../../../util/list.rkt")

(define now current-inexact-milliseconds)

(define-predicate ListofFixnum? (Listof Fixnum))

(define integer-col-1 (cons 'col1 (new-ISeries (assert (random-number-list 10000 200) ListofFixnum?))))

(define integer-col-2 (cons 'col2 (new-ISeries (assert (random-number-list 10000 200) ListofFixnum?))))

(define integer-col-3 (cons 'col3 (new-ISeries (assert (random-number-list 10000 200) ListofFixnum?))))

(define integer-col-4 (cons 'col4 (new-ISeries (assert (random-number-list 10000 200) ListofFixnum?))))


(define columns-integer
  (list integer-col-1 integer-col-2 integer-col-3 integer-col-4))

(define data-frame-integer (new-data-frame columns-integer))

(define integer-col-5 (cons 'col1 (new-ISeries (assert (random-number-list 10000 200) ListofFixnum?))))

(define integer-col-6 (cons 'col2 (new-ISeries (assert (random-number-list 10000 200) ListofFixnum?))))

(define integer-col-7 (cons 'col3 (new-ISeries (assert (random-number-list 10000 200) ListofFixnum?))))

(define integer-col-8 (cons 'col4 (new-ISeries (assert (random-number-list 10000 200) ListofFixnum?))))


;(define float-col-1 (cons 'float-col-1 (new-NSeries (flvector 1.5 2.5 3.5 4.5 5.5) #f)))

;(define categorical-col-1 (cons' categorical-col-1 (new-CSeries (vector 'a 'b 'c 'd 'e) #f)))

;(data-frame-write-tab data-frame-integer (current-output-port))


(displayln "data-frame-groupby")
;(data-frame-groupby data-frame-integer (list 'col1))

;(data-frame-groupby data-frame-integer (list 'col2))

;(data-frame-groupby data-frame-integer (list 'col1 'col2))

;(displayln "data-frame-groupby aggregate mean")
;(series-print (apply-agg-data-frame 'mean (data-frame-groupby data-frame-integer (list 'col1 'col2))) (current-output-port))

;(displayln "data-frame-groupby aggregate count")
;(series-print (apply-agg-data-frame 'count (data-frame-groupby data-frame-integer (list 'col1 'col2))) (current-output-port))

(define columns-integer-2
  (list integer-col-5 integer-col-6 integer-col-7 integer-col-8))

(define data-frame-integer-2 (new-data-frame columns-integer-2))

(displayln "data-frame left join")
(define data-frame-join-left-bench-before (now))
(data-frame-join-left data-frame-integer data-frame-integer-2 #:on (list 'col3))
(define data-frame-join-left-bench-after (- (now) data-frame-join-left-bench-before))
(fprintf (current-output-port)
         "data-frame left join bench ~v ms.\n"
         data-frame-join-left-bench-after)

(displayln "data-frame inner join")
(define data-frame-join-inner-bench-before (now))
(data-frame-join-inner data-frame-integer data-frame-integer-2 #:on (list 'col2))
(define data-frame-join-inner-bench-after (- (now) data-frame-join-inner-bench-before))
(fprintf (current-output-port)
         "data-frame inner join bench ~v ms.\n"
         data-frame-join-inner-bench-after)

(displayln "data-frame right join")
(define data-frame-join-right-bench-before (now))
(data-frame-join-right data-frame-integer data-frame-integer-2 #:on (list 'col2))
(define data-frame-join-right-bench-after (- (now) data-frame-join-right-bench-before))
(fprintf (current-output-port)
         "data-frame right join bench ~v ms.\n"
         data-frame-join-right-bench-after)

(displayln "data-frame outer join")
(define data-frame-join-outer-bench-before (now))
(define data-frame-outer-join-result (data-frame-join-outer data-frame-integer data-frame-integer-2 #:on (list 'col2)))
(define data-frame-join-outer-bench-after (- (now) data-frame-join-outer-bench-before))
(fprintf (current-output-port)
         "data-frame outer join bench ~v ms.\n"
         data-frame-join-outer-bench-after)


(displayln "data-frame multi-column left join")
(define data-frame-multi-column-join-left-bench-before (now))
(define data-frame-left-multi-join-result (data-frame-join-left data-frame-integer data-frame-integer-2 #:on (list 'col2 'col3)))
(define data-frame-multi-column-join-left-bench-after (- (now) data-frame-multi-column-join-left-bench-before))
(fprintf (current-output-port)
         "data-frame multi-column left join bench ~v ms.\n"
         data-frame-multi-column-join-left-bench-after)

(displayln "data-frame multi-column inner join")
(define data-frame-multi-column-join-inner-bench-before (now))
(define data-frame-inner-multi-join-result (data-frame-join-inner data-frame-integer data-frame-integer-2 #:on (list 'col2 'col3)))
(define data-frame-multi-column-join-inner-bench-after (- (now) data-frame-multi-column-join-inner-bench-before))
(fprintf (current-output-port)
         "data-frame multi-column inner join bench ~v ms.\n"
         data-frame-multi-column-join-inner-bench-after)

(displayln "data-frame multi-column right join")
(define data-frame-multi-column-join-right-bench-before (now))
(define data-frame-right-multi-join-result (data-frame-join-right data-frame-integer data-frame-integer-2 #:on (list 'col2 'col3)))
(define data-frame-multi-column-join-right-bench-after (- (now) data-frame-multi-column-join-right-bench-before))
(fprintf (current-output-port)
         "data-frame multi-column right join bench ~v ms.\n"
         data-frame-multi-column-join-right-bench-after)

(displayln "data-frame multi-column outer join")
(define data-frame-multi-column-join-outer-bench-before (now))
(define data-frame-outer-multi-join-result (data-frame-join-outer data-frame-integer data-frame-integer-2 #:on (list 'col2 'col3)))
(define data-frame-multi-column-join-outer-bench-after (- (now) data-frame-multi-column-join-outer-bench-before))
(fprintf (current-output-port)
         "data-frame multi-column outer join bench ~v ms.\n"
         data-frame-multi-column-join-outer-bench-after)

(displayln "data-frame even filter")
(define data-frame-column-filter-even-bench-before (now))
(data-frame-head (data-frame-column-filter data-frame-integer (lambda ([x : Any]) (even? (assert x index?))) 'col1))
(define data-frame-column-filter-even-bench-after (- (now) data-frame-column-filter-even-bench-before))
(fprintf (current-output-port)
         "data-frame even filter bench ~v ms.\n"
         data-frame-column-filter-even-bench-after)

(displayln "data-frame odd filter")
(define data-frame-column-filter-odd-bench-before (now))
(data-frame-head (data-frame-column-filter data-frame-integer (lambda ([x : Any]) (odd? (assert x index?))) 'col1))
(define data-frame-column-filter-odd-bench-after (- (now) data-frame-column-filter-odd-bench-before))
(fprintf (current-output-port)
         "data-frame odd filter bench ~v ms.\n"
         data-frame-column-filter-odd-bench-after)