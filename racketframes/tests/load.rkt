;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*    Author: Shubham Kahal
;*    File: load.rkt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket

(require typed/rackunit)

(require typed/db)

(require "../load/load.rkt")
(require "../data-frame/data-frame.rkt")
(require "../data-frame/data-frame-print.rkt")

; ***********************************************************
; Test Cases
; ***********************************************************
(define data-frame-from-sql-invoices (data-frame-from-sql (sqlite3-connect #:database "../validation/db/chinook.db") #f "SELECT * FROM invoices" empty))

(data-frame-head data-frame-from-sql-invoices)

(define data-frame-from-sql-customers (data-frame-from-sql (sqlite3-connect #:database "../validation/db/chinook.db") #f "SELECT * FROM customers" empty))

(data-frame-head data-frame-from-sql-customers)
