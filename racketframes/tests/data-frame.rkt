;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*    Author: Shubham Kahal
;*    File: tests/data-frame.rkt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket
(require typed/rackunit)

(require racket/fixnum)
(require racket/flonum)

(require "../data-frame/series-description.rkt")
(require "../data-frame/indexed-series.rkt")
(require "../data-frame/integer-series.rkt")
(require "../data-frame/numeric-series.rkt")
(require "../data-frame/categorical-series.rkt")
(require "../data-frame/generic-series.rkt")
(require "../data-frame/data-frame.rkt")
(require "../data-frame/data-frame-print.rkt")

; ***********************************************************
; Test Cases
; ***********************************************************
; data frame tests

;******************
;data-frame-integer
;******************
(define columns-integer
  (list 
   (cons 'col1 (new-ISeries (fxvector 1 2 3 4)
                            #:index (build-index-from-list (list 'a 'b 'c 'd))))
   (cons 'col2 (new-ISeries (fxvector 5 6 7 8)
                            #:index (build-index-from-list (list 'e 'f 'g 'h))))
   (cons 'col3 (new-ISeries (fxvector 9 10 11 12)
                            #:index (build-index-from-list (list 'i 'j 'k 'l))))))

; create new data-frame-integer
(define data-frame-integer (new-data-frame columns-integer))

(data-frame-iloc data-frame-integer (list 1 2) 2)

(data-frame-write-tab data-frame-integer (current-output-port))

;******************
;data-frame-float
;******************
(define columns-float
  (list 
   (cons 'col1 (new-NSeries (flvector 1.5 2.5 3.5 4.5)
                            #:index (build-index-from-list (list 'a 'b 'c 'd))))
   (cons 'col2 (new-NSeries (flvector 5.5 6.5 7.5 8.5)
                            #:index (build-index-from-list (list 'e 'f 'g 'h))))
   (cons 'col3 (new-NSeries (flvector 9.5 10.5 11.5 12.5)
                            #:index (build-index-from-list (list 'i 'j 'k 'l))))))

; create new data-frame-float
(define data-frame-float (new-data-frame columns-float))

(set! data-frame-float (data-frame-set-index data-frame-float (list 'a 'b 'c 'd)))

(hash->list (LabelIndex-index data-frame-float))

(label-index (LabelIndex-index data-frame-float) 'col1)

(data-frame-write-tab (assert (data-frame-iloc data-frame-float (list 1 2) (list 0 1)) DataFrame?) (current-output-port))

(data-frame-write-tab data-frame-float (current-output-port))

;******************
;data-frame-categorical
;******************
; will define parse to automatically build this columns structure
(define columns-categorical
  (list 
   (cons 'col1 (new-CSeries (vector 'hello 'world)))
   (cons 'col2 (new-CSeries (vector 'fizz 'buzz)))
   (cons 'col3 (new-CSeries (vector 'foo 'bar)))))

; create new data-frame-categorical
(define data-frame-categorical (new-data-frame columns-categorical))

(data-frame-write-tab data-frame-categorical (current-output-port))

;******************
;data-frame-mix
;******************
(define columns-mix
  (list
   (cons 'integer-col (new-ISeries (vector 1 2 3 4)
                            #:index (build-index-from-list (list 'a 'b 'c 'd))))
   (cons 'float-col (new-NSeries (flvector 1.5 2.5 3.5 4.5)
                            #:index (build-index-from-list (list 'a 'b 'c 'd))))
   (cons 'categorical-col (new-CSeries (vector 'hello 'world 'fizz 'buzz)))))

; create new data-frame-mix
(define data-frame-mix (new-data-frame columns-mix))

(data-frame-write-tab data-frame-mix (current-output-port))

; ************************
; data-frame-integer tests
; ************************

(check-equal? (series-data (data-frame-series-ref data-frame-integer 'col1))
              (vector 1 2 3 4))

(check-equal? (series-data (data-frame-series-ref data-frame-integer 'col2))
              (vector 5 6 7 8))

(set! data-frame-integer (data-frame-rename data-frame-integer 'col1 'col-one))

(check-equal? (data-frame-names data-frame-integer) (list 'col-one 'col2 'col3))

(check-equal? (series-data (data-frame-series-ref data-frame-integer 'col-one))
              (vector 1 2 3 4))

(check-equal? (data-frame-labels data-frame-integer)
              (list '(col2 1) '(col3 2) '(col-one 0)))

(check-equal? (projection-set (list 'col-one 'col2 'col3))
              (set 'col-one 'col2 'col3))

(check-equal? (data-frame-all-labels-projection-set data-frame-integer)
              (set 'col-one 'col2 'col3))

; check error
;(data-frame-series data-frame-integer 'col1)

(set! data-frame-integer (data-frame-drop data-frame-integer 'col-one))

; check col-one is gone

; ************************
; data-frame-float tests
; ************************

(check-equal? (series-data (data-frame-series-ref data-frame-float 'col1))
              (flvector 1.5 2.5 3.5 4.5))

(check-equal? (series-data (data-frame-series-ref data-frame-float 'col2))
              (flvector 5.5 6.5 7.5 8.5))

(set! data-frame-float (data-frame-rename data-frame-float 'col1 'col-one))

(check-equal? (data-frame-names data-frame-float) (list 'col-one 'col2 'col3))

(check-equal? (series-data (data-frame-series-ref data-frame-float 'col-one))
              (flvector 1.5 2.5 3.5 4.5))

; check error
;(data-frame-series data-frame-float 'col1)

(set! data-frame-float (data-frame-drop data-frame-float 'col-one))

; check col-one is gone

(data-frame-write-tab data-frame-float (current-output-port))

; ************************
; data-frame-categorical tests
; ************************

(check-equal? (series-data (data-frame-series-ref data-frame-categorical 'col1))
              (vector 'hello 'world))

(check-equal? (series-data (data-frame-series-ref data-frame-categorical 'col2))
              (vector 'fizz 'buzz))

(set! data-frame-categorical (data-frame-rename data-frame-categorical 'col1 'col-one))

(check-equal? (data-frame-names data-frame-categorical) (list 'col-one 'col2 'col3))

(check-equal? (series-data (data-frame-series-ref data-frame-categorical 'col-one))
              (vector 'hello 'world))

; check error
;(data-frame-series data-frame-float 'col1)

(set! data-frame-categorical (data-frame-drop data-frame-categorical 'col-one))

; check col-one is gone
(check-equal? (data-frame-names data-frame-categorical) (list 'col2 'col3))

; ************************
; data-frame-mix tests
; ************************

(check-equal? (series-data (data-frame-series-ref data-frame-mix 'integer-col))
              (vector 1 2 3 4))

(check-equal? (series-data (data-frame-series-ref data-frame-mix 'float-col))
              (flvector 1.5 2.5 3.5 4.5))

(check-equal? (series-data (data-frame-series-ref data-frame-mix 'categorical-col))
              (vector 'hello 'world 'fizz 'buzz))

(set! data-frame-mix (data-frame-rename data-frame-mix 'float-col 'float-column))

(check-equal? (data-frame-names data-frame-mix) (list 'integer-col 'float-column 'categorical-col))

(check-equal? (series-data (data-frame-series-ref data-frame-mix 'float-column))
              (flvector 1.5 2.5 3.5 4.5))

; check error
;(data-frame-series data-frame-float 'col1)

(set! data-frame-mix (data-frame-drop data-frame-mix 'float-column))

; check float-column is gone

(check-equal? (data-frame-names data-frame-mix) (list 'integer-col 'categorical-col))

; data-frame-explode tests
(data-frame-explode data-frame-integer)

(data-frame-description data-frame-integer)

(show-data-frame-description (data-frame-description data-frame-integer))

(check-equal? (series-iloc (data-frame-series-ref data-frame-integer 'col3) 2) 11)

(set! data-frame-integer (data-frame-set-index data-frame-integer (list 'a 'b 'c 'd)))
;(LabelIndex-index (cdr (list-ref (data-frame-explode data-frame-integer) 0)))

(data-frame-write-tab data-frame-integer (current-output-port))

(check-equal? (series-data (data-frame-series-ref data-frame-integer 'col3)) (vector 9 10 11 12))

(check-equal? (series-loc (data-frame-series-ref data-frame-integer 'col3) 'c) 11)

(set! data-frame-integer (data-frame-set-index data-frame-integer (list 'ind1 'ind2 'ind3 'ind4)))

(check-equal? (series-loc (data-frame-series-ref data-frame-integer 'col2) 'ind2) 6)