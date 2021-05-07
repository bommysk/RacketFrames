;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*    Author: Shubham Kahal
;*    File: tests/data-frame-ops.rkt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket
(require typed/rackunit)

(require racket/flonum)

(require "../data-frame/indexed-series.rkt")
(require "../data-frame/integer-series.rkt")
(require "../data-frame/boolean-series.rkt")
(require "../data-frame/numeric-series.rkt")
(require "../data-frame/categorical-series.rkt")
(require "../data-frame/generic-series.rkt")
(require "../data-frame/data-frame.rkt")
(require "../data-frame/data-frame-ops.rkt")
(require "../data-frame/data-frame-print.rkt")

; ***********
; Test Cases
; ***********

(define columns-integer-1
  (list 
   (cons 'col1 (new-ISeries (vector 1 2 3 4) #f))
   (cons 'col2 (new-ISeries (vector 5 6 7 8) #f))
   (cons 'col3 (new-ISeries (vector 9 10 11 12) #f))
   (cons 'col4 (new-ISeries (vector 21 22 23 24) #f))))

(define columns-integer-2
  (list 
   (cons 'col1 (new-ISeries (vector 1 2 3 4) #f))
   (cons 'col2 (new-ISeries (vector 25 26 27 28) #f))
   (cons 'col3 (new-ISeries (vector 29 30 31 32) #f))
   (cons 'col4 (new-ISeries (vector 1 2 3 4) #f))))

; create new data-frame-integer-1
(define data-frame-integer-1 (new-data-frame columns-integer-1))

; create new data-frame-integer-2
(define data-frame-integer-2 (new-data-frame columns-integer-2))

(displayln "data-frame+ Test 1")

(data-frame-write-delim data-frame-integer-1 (current-output-port))

(data-frame-write-delim data-frame-integer-2 (current-output-port))

(data-frame-write-delim (data-frame+ data-frame-integer-1 data-frame-integer-2) (current-output-port))


(displayln "data-frame- Test 1")

(data-frame-write-delim data-frame-integer-1 (current-output-port))

(data-frame-write-delim data-frame-integer-2 (current-output-port))

(data-frame-write-delim (data-frame- data-frame-integer-1 data-frame-integer-2) (current-output-port))

(displayln "data-frame* Test 1")

(data-frame-write-delim data-frame-integer-1 (current-output-port))

(data-frame-write-delim data-frame-integer-2 (current-output-port))

(data-frame-write-delim (data-frame* data-frame-integer-1 data-frame-integer-2) (current-output-port))

(displayln "data-frame/ Test 1")

(data-frame-write-delim data-frame-integer-1 (current-output-port))

(data-frame-write-delim data-frame-integer-2 (current-output-port))

(data-frame-write-delim (data-frame/ data-frame-integer-1 data-frame-integer-2) (current-output-port))

(displayln "data-frame% Test 1")

(data-frame-write-delim data-frame-integer-1 (current-output-port))

(data-frame-write-delim data-frame-integer-2 (current-output-port))

(data-frame-write-delim (data-frame% data-frame-integer-1 data-frame-integer-2) (current-output-port))

(displayln "data-frame-r Test 1")

(data-frame-write-delim data-frame-integer-1 (current-output-port))

(data-frame-write-delim data-frame-integer-2 (current-output-port))

(data-frame-write-delim (data-frame-r data-frame-integer-1 data-frame-integer-2) (current-output-port))

(displayln "data-frame= Test 1")

(data-frame-write-delim data-frame-integer-1 (current-output-port))

(data-frame-write-delim data-frame-integer-2 (current-output-port))

(data-frame-write-delim (data-frame= data-frame-integer-1 data-frame-integer-2) (current-output-port))

(displayln "data-frame!= Test 1")

(data-frame-write-delim data-frame-integer-1 (current-output-port))

(data-frame-write-delim data-frame-integer-2 (current-output-port))

(data-frame-write-delim (data-frame!= data-frame-integer-1 data-frame-integer-2) (current-output-port))

(displayln "data-frame< Test 1")

(data-frame-write-delim data-frame-integer-1 (current-output-port))

(data-frame-write-delim data-frame-integer-2 (current-output-port))

(data-frame-write-delim (data-frame< data-frame-integer-1 data-frame-integer-2) (current-output-port))

(displayln "data-frame> Test 1")

(data-frame-write-delim data-frame-integer-1 (current-output-port))

(data-frame-write-delim data-frame-integer-2 (current-output-port))

(data-frame-write-delim (data-frame> data-frame-integer-1 data-frame-integer-2) (current-output-port))

(displayln "data-frame<= Test 1")

(data-frame-write-delim data-frame-integer-1 (current-output-port))

(data-frame-write-delim data-frame-integer-2 (current-output-port))

(data-frame-write-delim (data-frame<= data-frame-integer-1 data-frame-integer-2) (current-output-port))

(displayln "data-frame>= Test 1")

(data-frame-write-delim data-frame-integer-1 (current-output-port))

(data-frame-write-delim data-frame-integer-2 (current-output-port))

(data-frame-write-delim (data-frame>= data-frame-integer-1 data-frame-integer-2) (current-output-port))

(displayln "data-frame-abs Test 1")

(data-frame-write-delim data-frame-integer-1 (current-output-port))

(define columns-integer-3
  (list 
   (cons 'col1 (new-ISeries (vector -1 -2 -3 -4) #f))
   (cons 'col2 (new-ISeries (vector -5 -6 -7 -8) #f))
   (cons 'col3 (new-ISeries (vector -9 -10 -11 -12) #f))
   (cons 'col4 (new-ISeries (vector -21 -22 -23 -24) #f))))

; create new data-frame-integer-3
(define data-frame-integer-3 (new-data-frame columns-integer-3))

(data-frame-write-delim data-frame-integer-3 (current-output-port))

(data-frame-write-delim (data-frame-abs data-frame-integer-1) (current-output-port))

(data-frame-write-delim (data-frame-abs data-frame-integer-3) (current-output-port))

(displayln "data-frame-filter test")

(displayln "data-frame-integer-3")
(data-frame-write-delim data-frame-integer-3 (current-output-port))

; data-frame-filter tests
(displayln "data-frame-filter result")
(data-frame-write-delim (data-frame-filter data-frame-integer-3 (new-BSeries (vector #f #t #t #f) #f)) (current-output-port))
