#lang racket/base

; SPDX-License-Identifier: BlueOak-1.0.0
; This file is licensed under the Blue Oak Model License 1.0.0.

;; Convenience/helper functions for this project’s Scribble documentation

(require scribble/core
         scribble/manual/lang
         scribble/html-properties
         scribble/private/manual-sprop
         scribble/decode
         racket/runtime-path
         (only-in net/uri-codec uri-encode))
(provide (all-defined-out))

(define-runtime-path custom-css "custom.css")

(define repo-url/ "https://thelocalyarn.com/code/")

;; Link to a ticket on the Fossil repository by specifying the ticket ID.
;; The "_parent" target breaks out of the iframe used by the Fossil repo web UI.
(define (ticket id-str)
  (hyperlink (string-append repo-url/ "tktview?name=" id-str)
             "ticket "
             (tt id-str)
             #:style (style #f (list (attributes '((target . "_parent")))))))

;; Link to a wiki page on the Fossil repository by specifying the title
(define (wiki title)
  (hyperlink (string-append repo-url/ "wiki?name=" (uri-encode title))
             title
             #:style (style #f (list (attributes '((target . "_parent")))))))

;; Link somewhere outside these docs or Racket docs. The `_blank` target opens in a new tab.
(define (ext-link url-str . elems)
  (keyword-apply hyperlink '(#:style) (list (style #f (list (attributes '((target . "_blank")))))) 
                 url-str
                 elems))

;; Link to show contents of the latest checked-in version of a file
;; (or a file listing if a directory was specified)
(define (repo-file filename)
  (hyperlink (string-append repo-url/ "file/" filename)
             (tt filename)
             #:style (style #f (list (attributes '((target . "_parent")))))))

(define (responsive-retina-image img-path)
  (image img-path 
         #:scale 0.5
         #:style (style #f (list (attributes '((style . "max-width:100%;height:auto;")))))))

;;
;; From https://github.com/mbutterick/pollen/blob/master/pollen/scribblings/mb-tools.rkt
;;

(define (terminal . args)
  (compound-paragraph (style "terminal" (list (css-style-addition custom-css) (alt-tag "div")))
                      (list (apply verbatim args))))

(define (cmd . args)
  (elem #:style (style #f (list (color-property "black"))) (tt args)))

(define (fileblock filename . inside)
  (compound-paragraph 
   (style "fileblock" (list* (alt-tag "div") 'multicommand
                             (box-mode "RfileboxBoxT" "RfileboxBoxC" "RfileboxBoxB") 
                             scheme-properties))
   (list
    (paragraph (style "fileblock_filetitle" (list* (alt-tag "div") (box-mode* "RfiletitleBox") scheme-properties))
               (list (make-element
                      (style "fileblock_filename" (list (css-style-addition custom-css)))
                      (if (string? filename)
                          (filepath filename)
                          filename))))
    (compound-paragraph 
     (style "fileblock_filecontent" (list* (alt-tag "div") (box-mode* "RfilecontentBox") scheme-properties))
     (decode-flow inside)))))

