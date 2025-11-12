#lang typed/racket/base

(require
 syntax/parse
 (only-in "layout-types.rkt"
	  LayoutTypeCode LayoutType))

(provide:
 [tr-type-for-field-type (LayoutTypeCode -> LayoutType)])

(: tr-type-for-field-type (LayoutTypeCode -> LayoutType))
(define (tr-type-for-field-type ftype)
  (case ftype
    ((I) 'Integer)
    ((C) 'String)
    ((N) 'Number)
    ((D) 'String)
    ((S) 'Symbol)))
