;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*    Author: Shubham Kahal
;*    File: tests/data-frame-concat.rkt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket
(require typed/rackunit)
(require racket/flonum)

(require "../data-frame/integer-series.rkt")
(require "../data-frame/numeric-series.rkt")
(require "../data-frame/categorical-series.rkt")
(require "../data-frame/data-frame.rkt")
(require "../data-frame/data-frame-concat.rkt")
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

; create new data-frame-integer-2
(define data-frame-integer-1 (new-data-frame columns-integer-1))

; create new data-frame-integer-3
(define data-frame-integer-2 (new-data-frame columns-integer-2))

(displayln "Concat Test 1")

(data-frame-write-delim data-frame-integer-1 (current-output-port))

(data-frame-write-delim data-frame-integer-2 (current-output-port))

(data-frame-write-delim (data-frame-concat-vertical data-frame-integer-1 data-frame-integer-2) (current-output-port))

(define columns-mixed-1
  (list 
   (cons 'col1 (new-ISeries (vector 1 2 3 4) #f))
   (cons 'col2 (new-NSeries (flvector 5.2 6.2 7.2 8.2) #f))
   (cons 'col3 (new-CSeries (vector 'a 'b 'c 'd) #f))
   (cons 'col4 (new-ISeries (vector 21 22 23 24) #f))))

(define columns-mixed-2
  (list 
   (cons 'col1 (new-ISeries (vector 1 2 3 4) #f))
   (cons 'col2 (new-NSeries (flvector 25.3 26.3 27.3 28.3) #f))
   (cons 'col3 (new-CSeries (vector 'e 'f 'g 'h) #f))
   (cons 'col4 (new-ISeries (vector 1 2 3 4) #f))))

; create new data-frame-mixed-1
(define data-frame-mixed-1 (new-data-frame columns-mixed-1))

; create new data-frame-mixed-2
(define data-frame-mixed-2 (new-data-frame columns-mixed-2))

(displayln "Concat Test")

(data-frame-write-delim data-frame-mixed-1 (current-output-port))

(data-frame-write-delim data-frame-mixed-2 (current-output-port))

(displayln "Vertical Concat")

(data-frame-write-delim (data-frame-concat-vertical data-frame-mixed-1 data-frame-mixed-2) (current-output-port))

(displayln "Horizontal Concat")

(data-frame-write-delim (data-frame-concat-horizontal data-frame-mixed-1 data-frame-mixed-2) (current-output-port))

(define columns-mixed-3
  (list 
   (cons 'col1 (new-ISeries (vector 1 2 3 4 5) #f))
   (cons 'col2 (new-NSeries (flvector 25.3 26.3 27.3 28.3 32.1) #f))
   (cons 'col3 (new-CSeries (vector 'e 'f 'g 'h 'i) #f))
   (cons 'col4 (new-ISeries (vector 1 2 3 4 7) #f))))

; create new data-frame-mixed-3
(define data-frame-mixed-3 (new-data-frame columns-mixed-3))

(data-frame-write-delim data-frame-mixed-3 (current-output-port))

(data-frame-write-delim (data-frame-concat-horizontal data-frame-mixed-2 data-frame-mixed-3) (current-output-port))
