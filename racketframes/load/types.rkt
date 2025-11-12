#lang typed/racket/base

(provide
 Line Line?
 LineParser
 ListofString
 ListofString?
 ListofLabel?)

(define-type Line String)
(define-type LineParser (Line -> (Listof String)))

(define-type ListofString (Listof String))

(define-type ListofAny (Listof Any))

(define Line? string?)

(define ListofString? (lambda ([x : ListofString]) : (U #f ListofString) (if (andmap string? x) x #f)))

(define ListofLabel? (lambda ([x : ListofAny]) : (U #f ListofAny) (if (andmap symbol? x) x #f)))
