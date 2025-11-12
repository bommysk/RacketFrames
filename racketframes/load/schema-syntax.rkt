#lang typed/racket

(provide
 schema)

(require
 "schema.rkt"
 (for-syntax syntax/parse))

(define-syntax (schema stx)

  (syntax-parse stx
		[(_ header?:boolean (name:id type:id) ...)
		 #'(Schema header?
			   (list (ColumnInfo 'name 'type) ...))]))
