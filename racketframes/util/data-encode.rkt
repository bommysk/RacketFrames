#lang typed/racket

(require racket/set
         racket/sequence)

(provide:
 [run-length-encode ((Listof Any) -> LengthEncodeType)]
 [scale-cdr (LengthEncodeType Positive-Integer -> (Listof NormalizedPair))]
 [elem-percent ((Listof Any) -> (Listof NormalizedPair))]
 [elem-percent> ((Listof Any) -> (Listof NormalizedPair))]
 [max-elem-percent ((Listof Any) -> NormalizedPair)]
 [max-elem ((Listof Any) -> Any)]
 [most-frequent-element-list ((Sequenceof Any) -> (Listof (Pairof Any Real)))]
 [most-frequent-element-count ((Sequenceof Any) -> (Pairof Any Real))]
 [most-frequent-elements ((Listof (Pairof Any Real)) -> (Listof Any))]
 [most-frequent-element ((Sequenceof Any) -> Any)]
 [min-elem-percent ((Listof Any) -> NormalizedPair)]
 [min-elem ((Listof Any) -> Any)]
 [least-frequent-element-list ((Sequenceof Any) -> (Listof (Pairof Any Real)))]
 [least-frequent-elements-sorted ((Listof (Pairof Any Real)) -> (Listof Any))]
 [least-frequent-element-count ((Sequenceof Any) -> (Pairof Any Real))]
 [least-frequent-element ((Sequenceof Any) -> Any)]
 [append-element ((Listof Any) Any -> (Listof Any))]
 [vector-indexes-of ((Vectorof Any) Any -> (Listof Index))]
 [vector-indexes-of-map ((Vectorof Any) -> (HashTable Any (Listof Index)))]
 [vector-indexes-of-set ((Vectorof Any) Any -> (Setof Index))]
 [vector-indexes-of-map-set ((Vectorof Any) -> (HashTable Any (Setof Index)))]
 [sequence-indexes-of ((Sequenceof Any) Any -> (Listof Index))]
 [sequence-indexes-of-map ((Sequenceof Any) -> (HashTable Any (Listof Index)))]
 [sequence-indexes-of-set ((Sequenceof Any) Any -> (Setof Index))]
 [sequence-indexes-of-map-set ((Sequenceof Any) -> (HashTable Any (Setof Index)))])

(provide LengthEncodePair LengthEncodeType LengthEncodeType? NormalizedPair NormalizedPair? ListofNormalizedPair ListofNormalizedPair?)

(define-predicate ListofIndex? (Listof Index))
(define-predicate SetofIndex? (Setof Index))

(define-type LengthEncodePair (Pairof Any Positive-Integer))
(define-type LengthEncodeType (U (Pairof LengthEncodePair (U (Listof LengthEncodePair) Null)) Null))
(define-predicate LengthEncodeType? LengthEncodeType)

(define-type NormalizedPair (Pairof Any Positive-Exact-Rational))
(define-predicate NormalizedPair? NormalizedPair)
(define-type ListofNormalizedPair (Listof NormalizedPair))
(define-predicate ListofNormalizedPair? ListofNormalizedPair)

(: run-length-encode ((Listof Any) -> LengthEncodeType))
(define (run-length-encode lst)
  (: rle ((Listof Any) Any Positive-Integer (U (Listof Any) Null) -> LengthEncodeType))
  (define (rle val-lst cur-val cur-cnt acc)
    (if (pair? val-lst)
        (let: ((new-val : Any (car val-lst)))
          (if (eq? new-val cur-val)
              (rle (cdr val-lst) cur-val (+ cur-cnt 1) acc)
              (rle (cdr val-lst) new-val 1 (cons (cons cur-val cur-cnt) acc))))
        (assert (cons (cons cur-val cur-cnt) acc) LengthEncodeType?)))
  (if (pair? lst)
      (reverse (rle (cdr lst) (car lst) 1 '()))
      '()))

(: scale-cdr (LengthEncodeType Positive-Integer -> (Listof NormalizedPair)))
(define (scale-cdr count-list total-count)
  (: normalize (LengthEncodePair -> NormalizedPair))
  (define (normalize pr)
    (cons (car pr) (/ (* 100 (cdr pr)) total-count)))
  (map normalize (assert count-list)))

(: elem-percent ((Listof Any) -> (Listof NormalizedPair)))
(define (elem-percent lst)
  (scale-cdr (run-length-encode (sort lst (lambda (val1 val2)
                                            (if (and (real? val1) (real? val2))
                                                (< val1 val2)
                                                (string<? (~v val1) (~v val2))))))
             ; doesn't make sense to run length encode on empty list
             (assert (length lst) exact-positive-integer?)))

; like elem-percent but in reverse order, i.e. >
(: elem-percent> ((Listof Any) -> (Listof NormalizedPair)))
(define (elem-percent> lst)
  (scale-cdr (run-length-encode (sort lst (lambda (val1 val2)
                                            (if (and (real? val1) (real? val2))
                                                (> val1 val2)
                                                (string>? (~v val1) (~v val2))))))
             ; doesn't make sense to run length encode on empty list
             (assert (length lst) exact-positive-integer?)))

(: max-elem-percent ((Listof Any) -> NormalizedPair))
(define (max-elem-percent lst)
  (argmax (lambda ((p : NormalizedPair)) (cdr p)) (assert (elem-percent lst) ListofNormalizedPair?)))

(: max-elem ((Listof Any) -> Any))
(define (max-elem lst)
  (car (max-elem-percent lst)))

(: most-frequent-element-list ((Sequenceof Any) -> (Listof (Pairof Any Real))))
(define (most-frequent-element-list xs)
  (: ht (HashTable Any Real))
  (define ht (make-hash))
  (for [(x xs)] (hash-update! ht x (λ ((b : Real)) (assert (add1 b) real?)) (λ () 0))) 
  (hash->list ht))

(: most-frequent-elements((Listof (Pairof Any Real)) -> (Listof Any)))
(define (most-frequent-elements element-counts)
  (map (λ ((x : (Pairof Any Real))) (car x)) element-counts))

(: most-frequent-element-count ((Sequenceof Any) -> (Pairof Any Real)))
(define (most-frequent-element-count xs)
  (argmax (λ ((x : (Pairof Any Real))) (cdr x)) 
          (most-frequent-element-list xs)))

(: most-frequent-element ((Sequenceof Any) -> Any))
(define (most-frequent-element xs)
  (car (most-frequent-element-count xs)))

(: min-elem-percent ((Listof Any) -> NormalizedPair))
(define (min-elem-percent lst)
  (argmin (lambda ((p : NormalizedPair)) (cdr p)) (assert (elem-percent lst) ListofNormalizedPair?)))

(: min-elem ((Listof Any) -> Any))
(define (min-elem lst)
  (car (min-elem-percent lst)))

(: least-frequent-element-list ((Sequenceof Any) -> (Listof (Pairof Any Real))))
(define (least-frequent-element-list xs)
  (: ht (HashTable Any Real))
  (define ht (make-hash))
  (for [(x xs)] (hash-update! ht x (λ ((b : Real)) (assert (add1 b) real?)) (λ () 0))) 
  (hash->list ht))

(: least-frequent-elements-sorted ((Listof (Pairof Any Real)) -> (Listof Any)))
(define (least-frequent-elements-sorted element-counts)
  (map (λ ((x : (Pairof Any Real))) (car x)) element-counts))

(: least-frequent-element-count ((Sequenceof Any) -> (Pairof Any Real)))
(define (least-frequent-element-count xs)
  (argmin (λ ((x : (Pairof Any Real))) (cdr x)) 
          (least-frequent-element-list xs)))

(: least-frequent-element ((Sequenceof Any) -> Any))
(define (least-frequent-element xs)
  (car (least-frequent-element-count xs)))

(: append-element ((Listof Any) Any -> (Listof Any)))
(define (append-element lst elem)
  (append lst (list elem)))

(: vector-indexes-of-map ((Vectorof Any) -> (HashTable Any (Listof Index))))
(define (vector-indexes-of-map vs)
  (: ht (HashTable Any (Listof Index)))
  (define ht (make-hash))
  (for [(elem vs) (i (in-range (vector-length vs)))]
    (hash-update! ht elem
                  (λ ((indexes : (Listof Index)))
                    (assert (append-element indexes (assert i index?)) ListofIndex?))
                  (λ () (assert (list) ListofIndex?))))
  ht)

; Consumes a vector and a value and returns all indexes in vector in order where
; the value occurs. Like list index-of, but returns the a list of all the indexes
; where the element occurs in the list instead of just the first one.
(: vector-indexes-of ((Vectorof Any) Any -> (Listof Index)))
(define (vector-indexes-of vs v)  
  (hash-ref (vector-indexes-of-map vs) v))

(: vector-indexes-of-map-set ((Vectorof Any) -> (HashTable Any (Setof Index))))
(define (vector-indexes-of-map-set vs)
  (: ht (HashTable Any (Setof Index)))
  (define ht (make-hash))
  (for [(elem vs) (i (in-range (vector-length vs)))]
    (hash-update! ht elem
                  (λ ((indexes : (Setof Index)))
                    (assert (set-add indexes (assert i index?)) SetofIndex?))
                  (λ () (assert (set) SetofIndex?))))
  ht)

; Consumes a vector and a value and returns all indexes in vector in order where
; the value occurs. Like list index-of, but returns the a list of all the indexes
; where the element occurs in the list instead of just the first one.
(: vector-indexes-of-set ((Vectorof Any) Any -> (Setof Index)))
(define (vector-indexes-of-set vs v)  
  (hash-ref (vector-indexes-of-map-set vs) v))

(: sequence-indexes-of-map ((Sequenceof Any) -> (HashTable Any (Listof Index))))
(define (sequence-indexes-of-map vs)
  (: ht (HashTable Any (Listof Index)))
  (define ht (make-hash))
  (for [(elem vs) (i (in-range (sequence-length vs)))]
    (hash-update! ht elem
                  (λ ((indexes : (Listof Index)))
                    (assert (append-element indexes (assert i index?)) ListofIndex?))
                  (λ () (assert (list) ListofIndex?))))
  ht)

; Consumes a vector and a value and returns all indexes in vector in order where
; the value occurs. Like list index-of, but returns the a list of all the indexes
; where the element occurs in the list instead of just the first one.
(: sequence-indexes-of ((Sequenceof Any) Any -> (Listof Index)))
(define (sequence-indexes-of vs v)  
  (hash-ref (sequence-indexes-of-map vs) v))

(: sequence-indexes-of-map-set ((Sequenceof Any) -> (HashTable Any (Setof Index))))
(define (sequence-indexes-of-map-set vs)
  (: ht (HashTable Any (Setof Index)))
  (define ht (make-hash))
  (for [(elem vs) (i (in-range (sequence-length vs)))]
    (hash-update! ht elem
                  (λ ((indexes : (Setof Index)))
                    (assert (set-add indexes (assert i index?)) SetofIndex?))
                  (λ () (assert (set) SetofIndex?))))
  ht)

; Consumes a vector and a value and returns all indexes in vector in order where
; the value occurs. Like list index-of, but returns the a list of all the indexes
; where the element occurs in the list instead of just the first one.
(: sequence-indexes-of-set ((Sequenceof Any) Any -> (Setof Index)))
(define (sequence-indexes-of-set vs v)  
  (hash-ref (sequence-indexes-of-map-set vs) v))