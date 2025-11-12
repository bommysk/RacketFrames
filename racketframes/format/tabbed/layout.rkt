#lang typed/racket/base

(provide
 define-tabbed-layout)

(require
 (only-in "../layout-types.rkt"
	  Field Layout)
 (for-syntax
  syntax/parse
  typed/racket
  (only-in racket/syntax
	   format-id)
  (only-in "../layout-types.rkt"
	   Field Layout)
  (only-in "../layout.rkt"
	   tr-type-for-field-type)))

(define-syntax (define-tabbed-layout stx)

  (define-syntax-class field
    (pattern (fid:id type:id)))

  (: field (Listof Field))
  (define fields '())

  (define (field-syntax fs)
    (for/list ([f fs])
	      (syntax-parse f
			    ((fid:id ftype:id)
			     (with-syntax ((tr-type (tr-type-for-field-type (syntax->datum #'ftype)))
					   (fname (syntax->datum #'fid)))
					  (set! fields (cons (Field (syntax->datum #'fid) 'String 0 0)
							     fields))
					  #`(Field 'fname 'tr-type 0 0))))))

  (syntax-parse stx
		[(_ name:id f0:field f1:field ...)
		 (with-syntax ((lo  #`(list #,@(field-syntax (syntax->list #'(f0 f1 ...)))))
			       (desc-name (format-id #'name "~a-desc" (syntax-e #'name))))
			      (let ((name-id (symbol->string (syntax->datum #'name))))
				#'(begin
				    (define-syntax desc-name (Layout 'name 'Tabbed lo))
				    (define name (Layout 'name 'Tabbed lo)))))]))
