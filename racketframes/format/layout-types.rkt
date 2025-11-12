#lang typed/racket/base

(provide
 LayoutTypeCode LayoutType
 (struct-out Field)
 (struct-out Layout))

(define-type LayoutTypeCode (U 'I 'C 'N 'D 'S))
(define-type LayoutType (U 'Integer 'String 'Number 'Date 'Symbol))

(struct: Field ([name : Symbol]
		[type : LayoutType]
		[offset : Natural]
		[length : Natural]))

(struct: Layout ([name   : Symbol]
		 [type   : (U 'Fixed 'Tabbed 'CSV)]
		 [fields : (Listof Field)]))
