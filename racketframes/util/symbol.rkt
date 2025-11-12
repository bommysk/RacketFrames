
#lang typed/racket

(provide:
 [symbol-prefix (Symbol String -> Symbol)])

(: symbol-prefix (Symbol String -> Symbol))
(define (symbol-prefix sym prefix)
  (string->symbol (string-append prefix (symbol->string sym))))
