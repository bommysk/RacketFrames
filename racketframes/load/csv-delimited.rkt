#lang typed/racket/base

;; why roll your own? use the csv-reading package and delete this module.

(provide
 parse-csv-line)

(provide:
 [read-csv-file (Path-String Boolean DataFrameBuilder -> DataFrameBuilder)])

(require
 (only-in "types.rkt"
	  LineParser)
 (only-in "delimited-common.rkt"
	  read-formatted-file)
 (only-in "data-frame-builder.rkt"
	  DataFrameBuilder))

; Self-Explanatory.
(: double-quote? (Char -> Boolean))
(define (double-quote? ch)
  (char=? #\" ch))

; Self-Explanatory.
(: comma-delimiter? (Char -> Boolean))
(define (comma-delimiter? ch)
  (char=? #\, ch))

; Self-Explanatory.
(: whitespace? (Char -> Boolean))
(define (whitespace? ch)
  (or (char=? ch #\space)
      (char=? ch #\tab)))

; This function consumes comma-delimiters else it just
; ignores and returns false.
(: consume-delimiter (Input-Port -> Boolean))
(define (consume-delimiter inp)
  (let ((ch (peek-char inp)))
    (if (char? ch)
	(if (comma-delimiter? ch)
	    (begin
	      (read-char inp)
	      #t)
	    #f)
	#f)))

; This function consumes an Input-Port and consumes all
; whitespace using a loop. If the character is not whitespace
; then void is returned.
(: maybe-consume-whitespace (Input-Port -> Void))
(define (maybe-consume-whitespace inp)
  (let loop ((ch (peek-char inp)))
    (if (eof-object? ch)
	(void)
	(if (whitespace? ch)
	    (begin
	      (read-char inp)
	      (loop (peek-char inp)))
	    (void)))))

(: trim-right (String -> String))
(define (trim-right s)
  (let ((idx (string-length s)))
    ;; No white space on right side of string, so just return string
    (if (and (> idx 0)
	     (not (whitespace? (string-ref s (sub1 idx))))) ;; fast path
	s
        ;; In loop keep looping as long as idx from end of string contains
        ;; whitespace. Once it doesn't just return substring, which will
        ;; be string with white space to the right trimmed off.
	(let loop ((idx idx))
	  (if (zero? idx)
	      s
	      (if (whitespace? (string-ref s (sub1 idx)))
		  (loop (sub1 idx))
		  (substring s 0 idx)))))))

;(displayln "trim-right")
;(trim-right "hello world      ")

; This function consumes an Input-Port and Output-Port and 
(: read-quoted-field (Input-Port Output-Port -> String))
(define (read-quoted-field inp outp)
  (read-char inp) ;; toss opening quote
  (let: loop : String ((ch : (U Char EOF) (read-char inp)))
	(if (eof-object? ch)
	    (trim-right (get-output-string outp)) ;; no closing quote, best effort.
	    (if (double-quote? ch)
		(let ((next-ch (peek-char inp)))
		  (if (eof-object? next-ch)
		      (trim-right (get-output-string outp))
		      (if (double-quote? next-ch)
			  (begin
			    (read-char inp)        ;; toss 2nd of "" in a row
			    (display next-ch outp) ;; write out just one "
			    (loop (read-char inp)))
			  (trim-right (get-output-string outp)))))
		(begin
		  (display ch outp)
		  (loop (read-char inp)))))))

;(displayln "read-quoted-field")
;(read-quoted-field (open-input-string "\"hello world\"") (open-output-string))
; input must have an opening quote, else the first character gets shaved off
;(read-quoted-field (open-input-string "hello world") (open-output-string))

(: read-unquoted-field (Input-Port Output-Port -> String))
(define (read-unquoted-field inp outp)
  (let: loop : String ((ch : (U Char EOF) (peek-char inp)))
	(if (eof-object? ch)
	    (get-output-string outp)
	    (if (comma-delimiter? ch)
		(trim-right (get-output-string outp))
		(begin
		  (display ch outp)
		  (read-char inp)
		  (loop (peek-char inp)))))))

;(displayln "read-unquoted-field")
;(read-unquoted-field (open-input-string "hello world  ") (open-output-string))

(: read-field (Input-Port -> String))
(define (read-field inp)
  (define outp (open-output-string))
  (let ((ch (peek-char inp)))
    (cond
     ((eof-object? ch)
      (get-output-string outp))
     ((double-quote? ch)
      (read-quoted-field inp outp))
     (else (read-unquoted-field inp outp)))))

;(read-field (open-input-string "hello world  "))
;(read-field (open-input-string "\"hello world  \""))
;(read-field (open-input-string "   hello world  "))

; This is the LineParser function, consume a String and
; returns a Listof String.
(: parse-csv-line LineParser)
(define (parse-csv-line line)
  (let ((inp (open-input-string line)))
    (maybe-consume-whitespace inp)
    (let loop ((fields (list (read-field inp))))
      (maybe-consume-whitespace inp)
      (if (consume-delimiter inp)
	  (loop (cons (read-field inp) fields))
	  (reverse fields)))))

(: read-csv-file (Path-String Boolean DataFrameBuilder -> DataFrameBuilder))
(define (read-csv-file path header? data-frame-builder)
  (begin (read-formatted-file path header? data-frame-builder parse-csv-line)
         data-frame-builder))
