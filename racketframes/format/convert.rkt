#lang typed/racket/base

(provide:
 [S->I (String -> Integer)]
 [S->F (String -> Float)])

(: S->I (String -> Integer))
(define (S->I s)
  (let ((n (string->number s)))
    (if (exact-integer? n)
	n
	(error "Expected Integer field: ~s" s))))

(: S->F (String -> Float))
(define (S->F s)
  (let ((n (string->number s)))
    (if (real? n)
	(real->double-flonum n)
	(error "Expected Real number field: ~s" s))))
