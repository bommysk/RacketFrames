#lang typed/racket

(provide 
 ~a ~r)

(define-type SignString (U String (List String String)))
(define-type SignTable (List SignString SignString SignString))

(require/typed racket/format
 (~a (String 
      [#:separator String]
      [#:width Natural]
      [#:max-width Natural]
      [#:min-width Natural]
      [#:limit-marker String]
      [#:align (U 'left 'right 'center)]
      [#:pad-string String]
      [#:left-pad-string String]
      [#:right-pad-string String] -> String))
 
 (~r (Real [#:sign (Option (U '+ '++ 'parens SignTable))]
	   [#:base Natural] ;; 2 - 36
	   [#:precision (U Natural (List '= Natural))]
	   [#:notation (U 'positional 'exponential)]
	   [#:format-exponent (Option (U String (Integer -> String)))]
	   [#:min-width Natural]
	   [#:pad-string String] -> String)))