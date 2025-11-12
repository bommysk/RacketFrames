#lang typed/racket

(provide:
 [generate-anon-series-names (Integer -> (Listof String))]
 [alter-schema-columns (Schema (Listof (Pair Symbol ColumnInfo)) -> Schema)]
 [schema-print (Schema Output-Port -> Void)]
 [alter-schema-no-headers (Schema -> Schema)])
 

(provide
 SeriesType
 (struct-out Schema)
 Schema-headers
 Schema-SeriesTypes
 (struct-out ColumnInfo))

(require
 racket/set
 (only-in "../data-frame/series-description.rkt" SeriesType))

;; Schema definition of a data file.
;; May be defined as meta information along with the data file
;; OR
;; Dynamically determined by sampling the data files.
(struct: ColumnInfo ([name : Symbol]
		     [type : SeriesType]) #:transparent)

(struct: Schema ([has-headers : Boolean]
		 [meta : (Listof ColumnInfo)])  #:transparent)

; This function consumes an Integer and produces a Listof String
; which represents anonymous series names simply created by appending
; an integer in a loop to the end of the string "x".
(: generate-anon-series-names (Integer -> (Listof String)))
(define (generate-anon-series-names n)
  (define base "x")
  (for/list ([x (in-range n)])
	    (string-append base (number->string x))))

; This function consumes a Schema struct and returns of Listof Symbol
; which represents the headers of the schema.
(: Schema-headers (Schema -> (Listof Symbol)))
(define (Schema-headers schema)
  (map (λ: ((meta : ColumnInfo))
	   (ColumnInfo-name meta))
       (Schema-meta schema)))

; This function consumes a Schema struct and returns of Listof SeriesType
; which represents the series types of the schema.
(: Schema-SeriesTypes (Schema -> (Listof SeriesType)))
(define (Schema-SeriesTypes schema)
  (map (λ: ((meta : ColumnInfo))
	   (ColumnInfo-type meta))
       (Schema-meta schema)))

; This function consumes a Schema and generates a new Schema replacing the
; headers of the passed in schema with anonymously generated headers.
(: alter-schema-no-headers (Schema -> Schema))
(define (alter-schema-no-headers schema)
  (if (Schema-has-headers schema)
      (let* ((metas (Schema-meta schema))
	     (hdrs (generate-anon-series-names (length metas))))
	(let: loop : Schema ((hdrs : (Listof Symbol) (map string->symbol hdrs))
			     (metas : (Listof ColumnInfo) metas)
			     (accum : (Listof ColumnInfo) '()))
	      (if (null? hdrs)
		  (Schema #f (reverse accum))
		  (loop (cdr hdrs) (cdr metas)
			(cons (ColumnInfo (car hdrs) (ColumnInfo-type (car metas)))
			      accum)))))
      schema))

; This function consumes a Schema and (Listof (Pair Symbol ColumnInfo)) and
; returns a new Schema
(: alter-schema-columns (Schema (Listof (Pair Symbol ColumnInfo)) -> Schema))
(define (alter-schema-columns schema metas)

  ; maps column name to its respective type of the passed in meta
  (define meta-map
    (let ((hmap ((inst make-hash Symbol ColumnInfo))))
      (for-each (λ: ((meta : (Pair Symbol ColumnInfo)))
		    (hash-set! hmap (car meta) (cdr meta)))
		metas)
      hmap))

  ; replace the metas of the passed in schema with the metas from
  ; the meta map built from the metas passed in
  (: replaced-metas (Listof ColumnInfo))
  (define replaced-metas (let: loop : (Listof ColumnInfo)
			       ((old-metas : (Listof ColumnInfo) (Schema-meta schema))
				(accum : (Listof ColumnInfo) '()))
			       (if (null? old-metas)
				   (reverse accum)
				   (let ((curr-meta (car old-metas)))
				     (loop (cdr old-metas)
					   (cons
					    (hash-ref meta-map
						      (ColumnInfo-name curr-meta)
						      (λ () curr-meta))
					    accum))))))

  (struct-copy Schema schema [meta replaced-metas]))


(: schema-print (Schema Output-Port -> Void))
(define (schema-print schema port)
  (begin
    (displayln "*******" port)
    (displayln "Schema" port)
    (displayln "*******" port)
    (displayln (Schema-headers schema) port)
    (displayln (Schema-SeriesTypes schema) port)))
