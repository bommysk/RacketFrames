;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*    Author: Shubham Kahal
;*    File: indexed-series.rkt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket

; ***********************************************************
; One-dimensional array like structure with axis labels. Labels
; must be unique and must be a hashable type. The series object
; supports both fixnum (idx) and label-based indexing. Functions
; can be mapped to each value of the series allowing for various
; operations. The series can hold any generic type.

; NOT IMPLEMENTED YET
; Operations between Series (+, -, /, *, **) align values based on their
; associated index values. The result index will be the sorted union of
; the two indexes.
; ***********************************************************

; ***********************************************************
; Provide functions in this file to other files.
(provide:
 [is-labeled? (LabelIndex -> Boolean)]
 [label-sort-positional (LabelIndex [#:project LabelProjection] -> Labeling)]
 [label-sort-lexical (LabelIndex -> Labeling)]
 [build-multi-index-from-list ((Listof (Listof GenericType)) -> LabelIndex)]
 [build-index-from-sequence ((Sequenceof IndexDataType) -> RFIndex)]
 [build-index-from-list ((Listof IndexDataType) -> RFIndex)]
 [build-data-vector-from-sequence ((Sequenceof GenericType) -> (Vectorof GenericType))]
 [labeling (LabelIndex -> (Listof (Pair Label (Listof Index))))]
 [get-index (RFIndex IndexDataType -> (Listof Index))]
 [extract-index (RFIndex -> IndexType)]
 [is-indexed? (RFIndex -> Boolean)]
 [index-keys (RFIndex -> (Listof IndexDataType))]
 [index-idxes (RFIndex -> (Listof Index))]
 [index-keys-intersection ((Listof RFIndex) -> (Setof IndexDataType))])

(provide
 SIndex IIndex FIndex DTIndex Labeling ListofLabel? ListofIndexDataType? ListofAny? ListofAnyPair?
 Label Label? LabelProjection LabelProjection? LabelIndex? RFIndex? Datetime?
 RFIndex RFNULL IndexDataType ListofIndex ListofIndex? ListofListofString ListofListofString?
 LabelIndex LabelIndex-index
 FIndex FlonumIndex ListofFixnum? ListofFlonum? ListofBoolean? ListofDatetime?
 build-index-from-labels label-index key->lst-idx label->lst-idx idx->key idx->label)
; ***********************************************************

; ***********************************************************
(require
  "../util/datetime.rkt"
  racket/sequence
  racket/set
  (only-in racket/flonum
           make-flvector flvector? flvector
           flvector-ref flvector-set!
           flvector-length))
; ***********************************************************

; ***********************************************************
(define-type Label Symbol)

; series have the option to set any type of null value
; and be converted to a GenSeries
(define-type RFNULL Any)

(define-predicate Label? Label)

(define-predicate ListofLabel? (Listof Label))

(define-predicate ListofFixnum? (Listof Fixnum))

(define-predicate SetofIndex? (Setof Index))

(define-predicate ListofIndex? (Listof Index))

(define-predicate ListofFlonum? (Listof Flonum))

(define-predicate ListofBoolean? (Listof Boolean))

(define-predicate ListofDatetime? (Listof Datetime))

(define-predicate ListofAny? (Listof Any))

(define-predicate ListofAnyPair? (Listof (Pair Any Any)))

; Map index to a  (Listof Fixnum)
(define-type Labeling (Listof (Pair Label (Listof Index))))

(define-type SIndex (HashTable Label (Listof Index)))

; Range Index
(define-type IIndex (HashTable Fixnum (Listof Index)))

(define-type FIndex (HashTable Flonum (Listof Index)))

(define-type DTIndex (HashTable Datetime (Listof Index)))

(define-type IndexDataType (U Label Fixnum Flonum Datetime))

(define-predicate ListofIndexDataType? (Listof IndexDataType))

(define-predicate SetofIndexDataType? (Setof IndexDataType))

(define-type LabelProjection (U (Listof Label) (Setof Label)))

(define-type ListofIndex (Listof Index))

(define-type ListofListofIndex (Listof (Listof Index)))

(define-predicate ListofListofIndex? (Listof (Listof Index)))

(define-type ListofListofString (Listof (Listof String)))

(define-predicate ListofListofString? (Listof (Listof String)))

(define-predicate LabelProjection? LabelProjection)

; like in Pandas, it could be dictionary of labels to values or not
; that's what the LabelIndex is for
(struct LabelIndex ([index : SIndex]) #:mutable)
(struct FixnumIndex ([index : IIndex]) #:mutable)
(struct FlonumIndex ([index : FIndex]) #:mutable)
(struct DatetimeIndex ([index : DTIndex]) #:mutable)

(define-type IndexType (U SIndex IIndex FIndex DTIndex))

; add RangeIndex later
(define-type RFIndex (U LabelIndex FixnumIndex FlonumIndex DatetimeIndex))

(define-predicate RFIndex? RFIndex)
; ***********************************************************

; ***********************************************************
; This function consumes a list of Labels and produces a
; SIndex which is a HashTable Label to Index.

(: build-index-from-sequence ((Sequenceof IndexDataType) -> RFIndex))
(define (build-index-from-sequence seq)
  (build-index-from-list (sequence->list seq)))

(: build-index-from-list ((Listof IndexDataType) -> RFIndex))
(define (build-index-from-list lst)
  (cond
    [(ListofLabel? lst) (LabelIndex (build-index-from-labels lst))]
    [(ListofFixnum? lst) (FixnumIndex (build-index-from-fixnums lst))]
    [(ListofFlonum? lst) (FlonumIndex (build-index-from-flonums lst))]
    [(ListofDatetime? lst) (DatetimeIndex (build-index-from-datetimes lst))]
    [else (error "Unsupported index datatype.")]))

(: build-data-vector-from-sequence ((Sequenceof GenericType) -> (Vectorof GenericType)))
(define (build-data-vector-from-sequence seq)
  (list->vector (sequence->list seq)))

(: get-index (RFIndex IndexDataType -> (Listof Index)))
(define (get-index index item)
  (cond
    [(and (LabelIndex? index) (Label? item)) (label-index (LabelIndex-index index) item)]
    [(and (FixnumIndex? index) (exact-integer? item)) (fixnum-index (FixnumIndex-index index) item)]
    [(and (FlonumIndex? index) (flonum? item)) (flonum-index (FlonumIndex-index index) item)]
    [(and (DatetimeIndex? index) (Datetime? item)) (datetime-index (DatetimeIndex-index index) item)]
    [else (error "Unsupported index datatype.")]))

(: extract-index (RFIndex -> IndexType))
(define (extract-index index)
  (cond
    [(LabelIndex? index) (LabelIndex-index index)]
    [(FixnumIndex? index) (FixnumIndex-index index)]
    [(FlonumIndex? index) (FlonumIndex-index index)]
    [(DatetimeIndex? index) (DatetimeIndex-index index)]
    [else (error "Unsupported index datatype.")]))

; This function consumes LabelIndex and Label and returns the
; numerical Index of the Label in the LabelIndex. The index
; must be a SIndex else an exception is raised.
(: is-indexed? (RFIndex -> Boolean))
(define (is-indexed? index)
  (cond
    [(LabelIndex? index) (is-labeled? index)]
    [(FixnumIndex? index) (has-fixnum-index? index)]
    [(FlonumIndex? index) (has-flonum-index? index)]
    [(DatetimeIndex? index) (has-datetime-index? index)]
    [else (error "Unsupported index datatype.")]))

(: key->lst-idx (RFIndex IndexDataType -> (Listof Index)))
(define (key->lst-idx index item)
  (cond
    [(LabelIndex? index) (label->lst-idx index (assert item Label?))]
    [(FixnumIndex? index) (fixnum->lst-idx index (assert item exact-integer?))]
    [(FlonumIndex? index) (flonum->lst-idx index (assert item flonum?))]
    [(DatetimeIndex? index) (datetime->lst-idx index (assert item Datetime?))]
    [else (error "Unsupported index datatype.")]))

; This function consumes LabelIndex and Label and returns the
; numerical Index of the Label in the LabelIndex. The index
; must be a SIndex else an exception is raised.
(: idx->key (RFIndex Index -> IndexDataType))
(define (idx->key index idx)
  (cond
    [(LabelIndex? index) (idx->label index idx)]
    [(FixnumIndex? index) (idx->fixnum index idx)]
    [(FlonumIndex? index) (idx->flonum index idx)]
    [(DatetimeIndex? index) (idx->datetime index idx)]
    [else (error "Unsupported index datatype.")]))

; perhaps put in utils
(: universal-sort< (GenericType GenericType -> Boolean))
(define (universal-sort< val1 val2)
  (if (and (real? val1) (real? val2))
      (< val1 val2)
      (string<? (~v val1) (~v val2))))

(: universal-sort> (GenericType GenericType -> Boolean))
(define (universal-sort> val1 val2)
  (if (and (real? val1) (real? val2))
      (> val1 val2)
      (string>? (~v val1) (~v val2))))
; perhaps put in utils

(: index-keys (RFIndex -> (Listof IndexDataType)))
(define (index-keys index)
  (sort (assert (hash-keys (extract-index index)) ListofIndexDataType?) universal-sort<))

(: index-keys-set (RFIndex -> (Setof IndexDataType)))
(define (index-keys-set index)
  (list->set (assert (hash-keys (extract-index index)) ListofIndexDataType?)))

(: index-idxes (RFIndex -> (Listof Index)))
(define (index-idxes index)
  (sort (assert (append* (assert (hash-values (extract-index index)) ListofListofIndex?)) ListofIndex?) universal-sort<))

(: list-intersect ((Listof GenericType) (Listof GenericType) -> (Setof GenericType)))
(define (list-intersect lst1 lst2)
  (let ((set1 (list->set lst1))
        (set2 (list->set lst2)))
    (set-intersect set1
                   set2)))
(: seq-intersect ((Sequenceof GenericType) (Sequenceof GenericType) -> (Setof GenericType)))
(define (seq-intersect seq1 seq2)
  (list-intersect (sequence->list seq1) (sequence->list seq2)))

(: index-keys-intersection ((Listof RFIndex) -> (Setof IndexDataType)))
(define (index-keys-intersection indexes)
  (assert (foldr (λ: ((set1 : (Setof IndexDataType)) (set2 : (Setof IndexDataType)))
                   (set-intersect set1 set2))
                 (index-keys-set (car indexes)) (map index-keys-set (cdr indexes)))
          SetofIndexDataType?))

(: list-union ((Listof GenericType) (Listof GenericType) -> (Setof GenericType)))
(define (list-union lst1 lst2)
  (let ((set1 (list->set lst1))
        (set2 (list->set lst2)))
    (set-union set1
                   set2)))
(: seq-union ((Sequenceof GenericType) (Sequenceof GenericType) -> (Setof GenericType)))
(define (seq-union seq1 seq2)
  (list-union (sequence->list seq1) (sequence->list seq2)))

; ***********************************************************

; ***********************************************************
; This function consumes a list of Labels and produces a
; SIndex which is a HashTable Label to Index.
(: build-index-from-labels ((Listof Label) -> SIndex))
(define (build-index-from-labels labels)
  (let ((index : SIndex (make-hash '())))
    (let loop : SIndex ((idx : Index 0) (labels : (Listof Label) labels))
      (if (null? labels)
          index
          (begin          
            (hash-update! index (car labels)
                          (λ: ((lst-index : (Listof Index)))
                            (append lst-index (list idx)))
                          (λ () (list)))

            
            (loop (assert (+ idx 1) index?) (cdr labels)))))))

(: label-index (SIndex Label -> (Listof Index)))
(define (label-index index label)      
  (hash-ref index label))
; ***********************************************************

; ***********************************************************
; This function consumes a list of Labels and produces a
; IIndex which is a HashTable Fixnum to Index.
(: build-index-from-fixnums ((Listof Fixnum) -> IIndex))
(define (build-index-from-fixnums fixnums)
  (let ((index : IIndex (make-hash '())))
    (let loop : IIndex ((idx : Index 0) (fixnums : (Listof Fixnum) fixnums))
      (if (null? fixnums)
          index
          (begin
            (hash-update! index (car fixnums)
                          (λ: ((lst-index : (Listof Index)))
                            (append lst-index (list idx)))
                          (λ () (list)))

            
            (loop (assert (+ idx 1) index?) (cdr fixnums)))))))

(: fixnum-index (IIndex Fixnum -> (Listof Index)))
(define (fixnum-index index fixnum)      
  (hash-ref index fixnum))
; ***********************************************************

; ***********************************************************
; This function consumes a list of Flonums and produces a
; FIndex which is a HashTable Label to Index.
(: build-index-from-flonums ((Listof Flonum) -> FIndex))
(define (build-index-from-flonums Flonums)
  (let ((index : FIndex (make-hash '())))
    (let loop : FIndex ((idx : Index 0) (Flonums : (Listof Flonum) Flonums))
      (if (null? Flonums)
          index
          (begin
            (hash-update! index (car Flonums)
                          (λ: ((lst-index : (Listof Index)))
                            (append lst-index (list idx)))
                          (λ () (list)))

            
            (loop (assert (+ idx 1) index?) (cdr Flonums)))))))

(: flonum-index (FIndex Flonum -> (Listof Index)))
(define (flonum-index index Flonum)      
  (hash-ref index Flonum))
; ***********************************************************

; ***********************************************************
; This function consumes a list of Datetimes and produces a
; DTIndex which is a HashTable Label to Index.
(: build-index-from-datetimes ((Listof Datetime) -> DTIndex))
(define (build-index-from-datetimes datetimes)
  (let ((index : DTIndex (make-hash '())))
    (let loop : DTIndex ((idx : Index 0) (datetimes : (Listof Datetime) datetimes))
      (if (null? datetimes)
          index
          (begin
            (hash-update! index (car datetimes)
                          (λ: ((lst-index : (Listof Index)))
                            (append lst-index (list idx)))
                          (λ () (list)))

            
            (loop (assert (+ idx 1) index?) (cdr datetimes)))))))

(: datetime-index (DTIndex Datetime -> (Listof Index)))
(define (datetime-index index datetime)      
  (hash-ref index datetime))

#|
B business day frequency
C custom business day frequency
D calendar day frequency
W weekly frequency
M month end frequency
SM  semi-month end frequency (15th and end of month)
BM  business month end frequency
CBM custom business month end frequency
MS  month start frequency
SMS semi-month start frequency (1st and 15th)
BMS business month start frequency
CBMS  custom business month start frequency
Q quarter end frequency
BQ  business quarter end frequency
QS  quarter start frequency
BQS business quarter start frequency
A, Y  year end frequency
BA, BY  business year end frequency
AS, YS  year start frequency
BAS, BYS  business year start frequency
BH  business hour frequency
H hourly frequency
T, min  minutely frequency
S secondly frequency
L, ms milliseconds
U, us microseconds
N nanoseconds
|#
; todo
;(: check-valid-freq (Label -> Boolean))

;(: date-range (Datetime Datetime [#:freq Label] -> DatetimeIndex))
;(define (date-range datetime-1 datetime-2 #:freq [freq 'D])
 ; (let ((day (Datetime-day datetime-1) (Datetime-day) 


;(: date-range-periods (Datetime Index [#:freq Label] -> DatetimeIndex))
; ***********************************************************

; ***********************************************************
#|
; General Series parameterized by A, allows for generic types.
; Numeric and categorical series will be further optimized.
(struct (A) GSeries LabelIndex ([data : (Vectorof A)]))

(: gseries-data (All (A) (GSeries A) -> (Vectorof A)))
(define (gseries-data gseries)
  (GSeries-data gseries))

(: get-total-index-value-count (SIndex -> Fixnum))
(define (get-total-index-value-count index)
  (define value-count 0)

  (for ([hash-val (hash-values index)])
    (set! value-count (+ value-count (length hash-val))))

  value-count)
  
; Consumes a Vector of generic type and a list of Labels which
; can come in list form or SIndex form and produces a GSeries
; struct object.
(: new-GSeries (All (A) (Vectorof A) (Option (U (Listof Label) SIndex)) -> (GSeries A)))
(define (new-GSeries data labels)
  
  (: check-mismatch (SIndex -> Void))
  (define (check-mismatch index)
    (unless (eq? (vector-length data) (get-total-index-value-count index))
      (let ((k (current-continuation-marks)))
        (raise (make-exn:fail:contract "Cardinality of a Series' data and labels must be equal" k))))
    (void))
  
  (if(hash? labels)
     (begin
       (check-mismatch labels)
       (GSeries labels data))
     (if labels  
         (let ((index (build-index-from-labels labels)))
           (check-mismatch index)
           (GSeries index data))
         (GSeries #f data)))) |#
; ***********************************************************

; ***********************************************************
; This function consumes a series and returns a boolean
; indicating whether series is a SIndex or not.
(: is-labeled? (LabelIndex -> Boolean))
(define (is-labeled? series)
  (if (LabelIndex-index series) #t #f))

; This function consumes LabelIndex and Label and returns the
; numerical Index of the Label in the LabelIndex. The index
; must be a SIndex else an exception is raised.
(: label->lst-idx (LabelIndex Label -> (Listof Index)))
(define (label->lst-idx series label)
  (let ((index (LabelIndex-index series)))
    (if index
        (hash-ref index label)
        (let ((k (current-continuation-marks)))
          (raise (make-exn:fail:contract "Cannot obtain the index of a label for a series which is unlabeled" k))))))

; This function consumes LabelIndex and Index and returns the
; Label at the numerical Index in the LabelIndex. The index
; must be a SIndex else an exception is raised.
(: idx->label (LabelIndex Index -> Label))
(define (idx->label series idx)
  (let ((index (LabelIndex-index series)))
    (if index
        (car (car (filter (lambda ([pair : (Pair Label (Listof Index))]) (member idx (cdr pair))) (hash->list index))))
        (let ((k (current-continuation-marks)))
          (raise (make-exn:fail:contract "Cannot obtain the index of a label for a series which is unlabeled" k))))))
; ***********************************************************

; ***********************************************************
; This function consumes a series and returns a boolean
; indicating whether series is a SIndex or not.
(: has-fixnum-index? (FixnumIndex -> Boolean))
(define (has-fixnum-index? index)
  (if (extract-index index) #t #f))

; This function consumes LabelIndex and Label and returns the
; numerical Index of the Label in the LabelIndex. The index
; must be a SIndex else an exception is raised.
(: fixnum->lst-idx (FixnumIndex Fixnum -> (Listof Index)))
(define (fixnum->lst-idx index fixnum)
  (let ((index : IIndex (FixnumIndex-index index)))
    (if index
        (hash-ref index fixnum)
        (let ((k (current-continuation-marks)))
          (raise (make-exn:fail:contract "Cannot obtain the index of a label for a series which is unlabeled" k))))))

; This function consumes LabelIndex and Label and returns the
; numerical Index of the Label in the LabelIndex. The index
; must be a SIndex else an exception is raised.
(: idx->fixnum (FixnumIndex Index -> Fixnum))
(define (idx->fixnum index idx)
  (let ((index : IIndex (FixnumIndex-index index)))
    (if index
        (car (car (filter (lambda ([pair : (Pair Fixnum (Listof Index))]) (member idx (cdr pair))) (hash->list index))))
        (let ((k (current-continuation-marks)))
          (raise (make-exn:fail:contract "Cannot obtain the index of a label for a series which is unlabeled" k))))))
; ***********************************************************

; ***********************************************************
; This function consumes a series and returns a boolean
; indicating whether series is a SIndex or not.
(: has-flonum-index? (FlonumIndex -> Boolean))
(define (has-flonum-index? index)
  (if (extract-index index) #t #f))

; This function consumes LabelIndex and Label and returns the
; numerical Index of the Label in the LabelIndex. The index
; must be a SIndex else an exception is raised.
(: flonum->lst-idx (FlonumIndex Flonum -> (Listof Index)))
(define (flonum->lst-idx index Flonum)
  (let ((index : FIndex (FlonumIndex-index index)))
    (if index
        (hash-ref index Flonum)
        (let ((k (current-continuation-marks)))
          (raise (make-exn:fail:contract "Cannot obtain the index of a label for a series which is unlabeled" k))))))

; This function consumes LabelIndex and Label and returns the
; numerical Index of the Label in the LabelIndex. The index
; must be a SIndex else an exception is raised.
(: idx->flonum (FlonumIndex Index -> Flonum))
(define (idx->flonum index idx)
  (let ((index : FIndex (FlonumIndex-index index)))
    (if index
        (car (car (filter (lambda ([pair : (Pair Flonum (Listof Index))]) (member idx (cdr pair))) (hash->list index))))
        (let ((k (current-continuation-marks)))
          (raise (make-exn:fail:contract "Cannot obtain the index of a label for a series which is unlabeled" k))))))
; ***********************************************************

; ***********************************************************
; This function consumes a series and returns a boolean
; indicating whether series is a SIndex or not.
(: has-datetime-index? (DatetimeIndex -> Boolean))
(define (has-datetime-index? index)
  (if (extract-index index) #t #f))

; This function consumes LabelIndex and Label and returns the
; numerical Index of the Label in the LabelIndex. The index
; must be a SIndex else an exception is raised.
(: datetime->lst-idx (DatetimeIndex Datetime -> (Listof Index)))
(define (datetime->lst-idx index datetime)
  (let ((index : DTIndex (DatetimeIndex-index index)))
    (if index
        (hash-ref index datetime)
        (let ((k (current-continuation-marks)))
          (raise (make-exn:fail:contract "Cannot obtain the index of a label for a series which is unlabeled" k))))))

; This function consumes LabelIndex and Label and returns the
; numerical Index of the Label in the LabelIndex. The index
; must be a SIndex else an exception is raised.
(: idx->datetime (DatetimeIndex Index -> Datetime))
(define (idx->datetime index idx)
  (let ((index : DTIndex (DatetimeIndex-index index)))
    (if index
        (car (car (filter (lambda ([pair : (Pair Datetime (Listof Index))]) (member idx (cdr pair))) (hash->list index))))
        (let ((k (current-continuation-marks)))
          (raise (make-exn:fail:contract "Cannot obtain the index of a label for a series which is unlabeled" k))))))
; ***********************************************************

#|
; ***********************************************************
; This function consumes a series and an Listof Index and returns
; the value at that index in the series.
(: gseries-iref (All (A) (GSeries A) (Listof Index) -> (U (Listof Flonum) (Listof A))))
(define (gseries-iref series lst-idx)
  (map (lambda ((idx : Index)) (vector-ref (GSeries-data series) idx))
       lst-idx))

; This function consumes a series and a Label and returns
; the value at that Label in the series.
(: series-ref (All (A) (GSeries A) Label -> (U (Listof A) (Listof Flonum))))
(define (series-ref series label)
  (gseries-iref series (label->lst-idx series label)))

; This function consumes a series and returns the length
; of that series.
(: gseries-length (All (A) (GSeries A) -> Index))
(define (gseries-length series)
  (vector-length (GSeries-data series)))
; ***********************************************************

; ***********************************************************
; This function consumes a series and a function both of generic
; types and applies the function to each member of the series
; returning a new series.
(: map/GSeries (All (A B) (GSeries A) (A -> B) -> (GSeries B)))
(define (map/GSeries series fn)
  (let*: ((old-data : (Vectorof A) (GSeries-data series))
          (new-data : (Vectorof B) (build-vector (vector-length old-data) 
                                                 (λ: ((idx : Fixnum)) 
                                                   (fn (vector-ref old-data idx))))))
    (GSeries (LabelIndex-index series) new-data)))
; ***********************************************************
|#

; ***********************************************************
; This function consumes a LabelIndex which as long as it is
; a valid SIndex which is a HashTable, it converts it to a
; list of Label (Listof Index) pairs.
(: labeling (LabelIndex -> (Listof (Pair Label (Listof Index)))))
(define (labeling lindex)
  (hash->list (LabelIndex-index lindex)))
; ***********************************************************

; ***********************************************************
; This function consumes a LabelIndex and produces a sorted
; Labeling which is a list consisting of Label Index pairs.
; The Labeling is sorted on the Label.
(: label-sort-lexical (LabelIndex -> Labeling))
(define (label-sort-lexical lindex)
  ((inst sort (Pair Label (Listof Index)) (Pair Label (Listof Index)))
   (labeling lindex)
   (λ: ((kv1 : (Pair Label (Listof Index)))
        (kv2 : (Pair Label (Listof Index))))
     (string<=? (symbol->string (car kv1))
                (symbol->string (car kv2))))))
; ***********************************************************

; ***********************************************************
; This function consumes a LabelIndex and LabelProjection produces
; a sorted Labeling which is a list consisting of Label Index pairs.
; The Labeling is sorted on the index of the labels.
(: label-sort-positional (LabelIndex [#:project LabelProjection] -> Labeling))
(define (label-sort-positional lindex #:project [project '()])

  (define: projection : (Setof Label) (if (list? project) (list->set project) project))
  
  (let ((labels ((inst sort (Pair Symbol (Listof Index)) (Pair Symbol (Listof Index)))
                 (labeling lindex)
                 (λ: ((kv1 : (Pair Symbol (Listof Index)))
                      (kv2 : (Pair Symbol (Listof Index))))
                   ; just comapres first of the list of index
                   (< (car (cdr kv1)) (car (cdr kv2)))))))

    (if (set-empty? projection)
        labels
        (filter (λ: ((label : (Pair Symbol (Listof Index))))
                  (set-member? projection (car label)))
                labels))))
; ***********************************************************

; ***********************************************************

(define-type GenericType Any)

(define key-delimiter "\t")

; This function consumes a Listof IndexableSeries and builds key
; string from the columns of a frame and a given set of col labels to use.
; Insert a tab char between each key value, e.g., k1 + \t + k2 + \t + ...
(: key-fn-list ((Listof (Listof GenericType)) -> (Index -> String)))
(define (key-fn-list lsts)
  (λ: ((row-id : Index))
    (let ((outp (open-output-string)))
      (for ([lst (in-list lsts)])
        (let*: ((seg : GenericType (list-ref lst row-id))
                (seg-str : String (cond
                                    [(symbol? seg) (symbol->string seg)]
                                    [(number? seg) (number->string seg)]
                                    ; pretty-format anything else
                                    [else (pretty-format seg)])))
          (display seg-str outp)
          (display key-delimiter outp)))
      (get-output-string outp))))

(: build-multi-index-from-sequence ((Sequenceof (Sequenceof GenericType)) -> LabelIndex))
(define (build-multi-index-from-sequence seq)
  (build-multi-index-from-list (sequence->list (sequence-map (lambda ([s : (Sequenceof GenericType)]) (sequence->list s)) seq))))

(: build-multi-index-from-list ((Listof (Listof GenericType)) -> LabelIndex))
(define (build-multi-index-from-list nested-label-lst)

  ; Get length of one of the IndexableSeries
  (define len (length (car nested-label-lst)))
  (define: series-key : (Index -> String) (key-fn-list nested-label-lst))

  (LabelIndex (let ((index : SIndex (make-hash '())))

    (let loop ([i 0])
      (if (>= i len)
          index
          (let: ((i : Index (assert i index?)))
            (let ((key (series-key i)))              
              (hash-update! index (string->symbol key)
                            (λ: ((idx : (Listof Index)))
                              (append idx (list i)))
                            (λ () (list))))
            (loop (add1 i))))))))

; ***********************************************************