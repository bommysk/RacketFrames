#lang typed/racket

(: vector-print ((Vectorof Any) Output-Port -> Void))
(define (vector-print vec port)
  (let ((len (vector-length vec))
	(out (current-output-port)))
    (if (zero? len)
	(displayln "[ ]" port)
	(begin
	  (display "[ " port)
	  (do ((i 0 (add1 i)))
	      ((>= i len) (void))
	    (let ((val (vector-ref vec i)))
	      (display val port))
	    (display " " port))))
    (display "]" port)))

