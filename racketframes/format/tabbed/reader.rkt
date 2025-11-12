#lang typed/racket/base

(provide: 
 [string-field  (Input-Port -> String)]
 [number-field  (Input-Port -> Number)]
 [integer-field (Input-Port -> Integer)]
 [float-field   (Input-Port -> Float)])

(: string-field (Input-Port -> String))
(define (string-field inp)
  (define outp (open-output-string))
  (let loop ((ch (read-char inp)))
    (if (eof-object? ch)
	(get-output-string outp)
	(if (char=? ch #\tab)
	    (get-output-string outp)
	    (begin
	      (display ch outp)
	      (loop (read-char inp)))))))
  
(: number-field (Input-Port -> Number))
(define (number-field inp)
  (let ((n (read inp)))
    (if (number? n)
	n
	(error "Expected Number value: ~s" n))))

(: integer-field (Input-Port -> Integer))
(define (integer-field inp)
  (let ((v (read inp)))
    (if (exact-integer? v)
	v
	(error "Expected Integer value: ~s" v))))

(: float-field (Input-Port -> Float))
(define (float-field inp)
  (let ((v (read inp)))
    (cond
     ((real? v)
      (real->double-flonum v))
     ((eof-object? v)
      +nan.0)
     (else      
      (error 'float-field "Expected Float value: ~s" v)))))
