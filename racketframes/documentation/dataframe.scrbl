#lang scribble/manual
@(require scribble/core)
@(require racket/runtime-path)

@require[(for-label racket/contract)]

@title[#:tag "top" #:version "1.0" #:style 'toc-hidden]{RacketFrames}

@author["Shubham Kahal"]

@verbatim{================================================================================}

@"\U2192" A DataFrame implementation in the spirit of Python Pandas and R DataFrames.

@"\U2192" In these data centric times, procedural and functional programmers alike are working
with large amounts of data. Many analytical libraries have been developed for a variety
domains to work with such data. Languages like Python and R or even the command line can be
used to accomplish analysis tasks, but RacketFrames is especially good if you are 1)
using Racket already 2) may need to integrate your solution with other Racket applications
in the future or 3) enjoy functional programming.

@verbatim{================================================================================}

@larger{@bold{Getting Started}}

Import library.
@codeblock|{
(require RacketFrames)
}|
Define columns.
@codeblock|{
(define columns-mix
  (list
   (cons 'integer-col (new-ISeries (vector 1 2 3 4)
                            (build-index-from-list (list 'a 'b 'c 'd))))
   (cons 'categorical-col (new-CSeries (vector 'hello 'world 'fizz 'buzz)
                                       (build-index-from-list (list 'a 'b 'c 'd))))))
}|
Initialize DataFrame.
@codeblock|{
; create new data-frame-mix
(define data-frame-mix (new-data-frame columns-mix))

; write data-frame-mix to stdout
(data-frame-write-tab data-frame-mix (current-output-port))
}|

@tabular[#:sep @hspace[1] #:row-properties '(bottom-border)
(list (list @bold{integer-col} @bold{categorical-col})
(list "1" "hello")
(list "2" "world")
(list "3" "fizz")
(list "4" "buzz"))]

Initialize DataFrame from CSV file.

@(define-runtime-path salary-date-csv-png "salary_date_csv.png")
@image[salary-date-csv-png]

@codeblock|{
; no schema
(define salary-data-frame-csv-no-schema (load-csv-file "../sample-csv/salary_date.csv" #:schema #f))

(data-frame-head salary-data-frame-csv-no-schema)
}|

@tabular[#:sep @hspace[1] #:row-properties '(bottom-border)
(list (list @bold{first} @bold{last} @bold{age} @bold{dollar} @bold{phone} @bold{join_date})
(list "Evan" "Lloyd"  "19" "$3839.78"  "(771) 255-1133"  "(Datetime (Date 2018 5 19) (Time 0 0 0 0 0))") 
(list "Etta" "Griffith" "50" "$8158.60" "(523) 731-6388" "(Datetime (Date 2018 5 19) (Time 0 0 0 0 0))") 
(list "William" "Conner" "50" "$9966.70" "(759) 504-6619"  "(Datetime (Date 2018 5 19) (Time 0 0 0 0 0))") 
(list "Rhoda" "Guerrero" "20" "$6480.10" "(467) 431-4273"  "(Datetime (Date 2018 5 19) (Time 0 0 0 0 0))") 
(list "Kyle" "Klein" "59" "$6106.13"  "(760) 829-2093"  "(Datetime (Date 2018 5 19) (Time 0 0 0 0 0))") 
(list "Benjamin" "Patton" "59" "$3925.51" "(488) 673-5745" "(Datetime (Date 2018 5 19) (Time 0 0 0 0 0))") 
(list "Georgie" "Hansen" "51" "$8809.92" "(579) 706-4402" "(Datetime (Date 2018 5 19) (Time 0 0 0 0 0))") 
(list "Gregory" "Bowen" "36" "$5176.21" "(533) 506-3845"  "(Datetime (Date 2018 5 19) (Time 0 0 0 0 0))") 
(list "Cornelia" "Peterson" "46" "$3626.31" "(861) 316-5672"  "(Datetime (Date 2018 5 19) (Time 0 0 0 0 0))") 
(list "Samuel" "Cole" "37" "$7677.20" "(760) 406-6331" "(Datetime (Date 2018 5 19) (Time 0 0 0 0 0))"))]

@codeblock|{
(displayln "List of Column Names")
(data-frame-names salary-data-frame-csv-no-schema)
}|

@verbatim|{
'(first last age dollar phone join_date)
}|

@codeblock|{
(displayln "DataFrame Dimensions")
(data-frame-dim salary-data-frame-csv-no-schema)
}|

@verbatim|{
DataFrame Dimensions
(Dim 200 6)}|

@codeblock|{
(displayln "DataFrame Description")
(show-data-frame-description (data-frame-description salary-data-frame-csv-no-schema))
}|

@verbatim|{
DataFrame Description
DataFrame::(Cols: 6, Rows: 200)
  - first: CategoricalSeries
  - last: CategoricalSeries
  - age: IntegerSeries
  - dollar: CategoricalSeries
  - phone: CategoricalSeries
  - join_date: DatetimeSeries}|

@codeblock|{
(displayln "DataFrame Remove")
(data-frame-head (data-frame-remove salary-data-frame-csv-no-schema (list 'first 'age)))
}|

@verbatim|{
DataFrame Remove
     last           dollar           phone         join_date    
Lloyd           $3839.78        (771) 255-1133  (Datetime (Date 2018 5 19) (Time 0 0 0 0 0)) 
Griffith        $8158.60        (523) 731-6388  (Datetime (Date 2018 5 19) (Time 0 0 0 0 0)) 
Conner          $9966.70        (759) 504-6619  (Datetime (Date 2018 5 19) (Time 0 0 0 0 0)) 
Guerrero        $6480.10        (467) 431-4273  (Datetime (Date 2018 5 19) (Time 0 0 0 0 0)) 
Klein           $6106.13        (760) 829-2093  (Datetime (Date 2018 5 19) (Time 0 0 0 0 0)) 
Patton          $3925.51        (488) 673-5745  (Datetime (Date 2018 5 19) (Time 0 0 0 0 0)) 
Hansen          $8809.92        (579) 706-4402  (Datetime (Date 2018 5 19) (Time 0 0 0 0 0)) 
Bowen           $5176.21        (533) 506-3845  (Datetime (Date 2018 5 19) (Time 0 0 0 0 0)) 
Peterson        $3626.31        (861) 316-5672  (Datetime (Date 2018 5 19) (Time 0 0 0 0 0)) 
Cole            $7677.20        (760) 406-6331  (Datetime (Date 2018 5 19) (Time 0 0 0 0 0))
}|

@codeblock|{
(displayln "DataFrame Project")
(data-frame-head (data-frame-project salary-data-frame-csv-no-schema (list 'first 'last 'dollar)))
}|

@verbatim|{
DataFrame Project
     first           last           dollar      
Evan            Lloyd           $3839.78        
Etta            Griffith        $8158.60        
William         Conner          $9966.70        
Rhoda           Guerrero        $6480.10        
Kyle            Klein           $6106.13        
Benjamin        Patton          $3925.51        
Georgie         Hansen          $8809.92        
Gregory         Bowen           $5176.21        
Cornelia        Peterson        $3626.31        
Samuel          Cole            $7677.20
}|

@codeblock|{
(displayln "DataFrame Replace")
(data-frame-head (data-frame-replace salary-data-frame-csv-no-schema (cons 'salary (new-CSeries (make-vector 200 '$0.00) #f))))
}|

@verbatim|{
DataFrame Replace
     first           last             age           dollar           phone         join_date    
Evan            Lloyd                        19 $3839.78        (771) 255-1133  (Datetime (Date 2018 5 19) (Time 0 0 0 0 0)) 
Etta            Griffith                     50 $8158.60        (523) 731-6388  (Datetime (Date 2018 5 19) (Time 0 0 0 0 0)) 
William         Conner                       50 $9966.70        (759) 504-6619  (Datetime (Date 2018 5 19) (Time 0 0 0 0 0)) 
Rhoda           Guerrero                     20 $6480.10        (467) 431-4273  (Datetime (Date 2018 5 19) (Time 0 0 0 0 0)) 
Kyle            Klein                        59 $6106.13        (760) 829-2093  (Datetime (Date 2018 5 19) (Time 0 0 0 0 0)) 
Benjamin        Patton                       59 $3925.51        (488) 673-5745  (Datetime (Date 2018 5 19) (Time 0 0 0 0 0)) 
Georgie         Hansen                       51 $8809.92        (579) 706-4402  (Datetime (Date 2018 5 19) (Time 0 0 0 0 0)) 
Gregory         Bowen                        36 $5176.21        (533) 506-3845  (Datetime (Date 2018 5 19) (Time 0 0 0 0 0)) 
Cornelia        Peterson                     46 $3626.31        (861) 316-5672  (Datetime (Date 2018 5 19) (Time 0 0 0 0 0)) 
Samuel          Cole                         37 $7677.20        (760) 406-6331  (Datetime (Date 2018 5 19) (Time 0 0 0 0 0))
}|

@codeblock|{
(displayln "DataFrame Replace Non Existent Column")
(data-frame-head (data-frame-replace salary-data-frame-csv-no-schema (cons 'dollar (new-CSeries (make-vector 200 '$0.00) #f))))
}|

@verbatim|{
DataFrame Replace Non Existent Column
     first           last             age           dollar           phone         join_date    
Evan            Lloyd                        19 $0.00           (771) 255-1133  (Datetime (Date 2018 5 19) (Time 0 0 0 0 0)) 
Etta            Griffith                     50 $0.00           (523) 731-6388  (Datetime (Date 2018 5 19) (Time 0 0 0 0 0)) 
William         Conner                       50 $0.00           (759) 504-6619  (Datetime (Date 2018 5 19) (Time 0 0 0 0 0)) 
Rhoda           Guerrero                     20 $0.00           (467) 431-4273  (Datetime (Date 2018 5 19) (Time 0 0 0 0 0)) 
Kyle            Klein                        59 $0.00           (760) 829-2093  (Datetime (Date 2018 5 19) (Time 0 0 0 0 0)) 
Benjamin        Patton                       59 $0.00           (488) 673-5745  (Datetime (Date 2018 5 19) (Time 0 0 0 0 0)) 
Georgie         Hansen                       51 $0.00           (579) 706-4402  (Datetime (Date 2018 5 19) (Time 0 0 0 0 0)) 
Gregory         Bowen                        36 $0.00           (533) 506-3845  (Datetime (Date 2018 5 19) (Time 0 0 0 0 0)) 
Cornelia        Peterson                     46 $0.00           (861) 316-5672  (Datetime (Date 2018 5 19) (Time 0 0 0 0 0)) 
Samuel          Cole                         37 $0.00           (760) 406-6331  (Datetime (Date 2018 5 19) (Time 0 0 0 0 0))
}|

@codeblock|{
(displayln "DataFrame Extend")
(data-frame-head (data-frame-extend salary-data-frame-csv-no-schema (cons 'state (new-CSeries (make-vector 200 'CA) #f))))
}|

@verbatim|{
DataFrame Extend
     first           last             age           dollar           phone         join_date         state      
Evan            Lloyd                        19 $3839.78        (771) 255-1133  (Datetime (Date 2018 5 19) (Time 0 0 0 0 0)) CA              
Etta            Griffith                     50 $8158.60        (523) 731-6388  (Datetime (Date 2018 5 19) (Time 0 0 0 0 0)) CA              
William         Conner                       50 $9966.70        (759) 504-6619  (Datetime (Date 2018 5 19) (Time 0 0 0 0 0)) CA              
Rhoda           Guerrero                     20 $6480.10        (467) 431-4273  (Datetime (Date 2018 5 19) (Time 0 0 0 0 0)) CA              
Kyle            Klein                        59 $6106.13        (760) 829-2093  (Datetime (Date 2018 5 19) (Time 0 0 0 0 0)) CA              
Benjamin        Patton                       59 $3925.51        (488) 673-5745  (Datetime (Date 2018 5 19) (Time 0 0 0 0 0)) CA              
Georgie         Hansen                       51 $8809.92        (579) 706-4402  (Datetime (Date 2018 5 19) (Time 0 0 0 0 0)) CA              
Gregory         Bowen                        36 $5176.21        (533) 506-3845  (Datetime (Date 2018 5 19) (Time 0 0 0 0 0)) CA              
Cornelia        Peterson                     46 $3626.31        (861) 316-5672  (Datetime (Date 2018 5 19) (Time 0 0 0 0 0)) CA              
Samuel          Cole                         37 $7677.20        (760) 406-6331  (Datetime (Date 2018 5 19) (Time 0 0 0 0 0)) CA
}|

@verbatim{================================================================================}

@bold{Table of Contents}

@table-of-contents[]

@; ======================================================================
@section[#:tag "Series"]{Series}

@itemlist[

 @item{One-dimensional ndarray with axis labels (including time series).

Labels need not be unique but must be a hashable type. The object supports both integer- and label-based indexing and provides a host of methods for performing operations involving the index. Statistical methods from ndarray have been overridden to automatically exclude missing data (currently represented as NaN).

Operations between Series (+, -, /, , *) align values based on their associated index values– they need not be the same length. The result index will be the sorted union of the two indexes.
}

]

@subsection[#:style 'toc]{Integer Series}

@local-table-of-contents[]

@subsubsection[#:tag "new-ISeries"]{new-ISeries}
@defproc[#:link-target? #f
 (new-ISeries [data (Vectorof Fixnum)] [label (Option (U (Listof Label) SIndex))])
ISeries?]{
Returns a new ISeries.
}

@codeblock|{
    (define series-integer (new-ISeries (vector 1 2 3 4)
                (build-index-from-labels (list 'a 'b 'c 'd))))
  }|

@subsubsection[#:tag "iseries-iref"]{iseries-iref}
@defproc[#:link-target? #f
 (iseries-iref [iseries ISeries] [idx Index])
Fixnum?]{
Returns the Fixnum value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-ISeries (vector 1 2 3 4)
  (build-index-from-labels (list 'a 'b 'c 'd))))

  (iseries-iref series-integer 0) ; 1

  (iseries-iref series-integer 1) ; 2
  }|

@subsubsection[#:tag "iseries-label-ref"]{iseries-label-ref}
@defproc[#:link-target? #f
 (iseries-label-ref [iseries ISeries] [label Label])
Fixnum?]{
Returns the Fixnum value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-ISeries (vector 1 2 3 4)
  (build-index-from-labels (list 'a 'b 'c 'd))))

  (iseries-label-ref series-integer 'd) ; 4

  (iseries-label-ref series-integer 'c) ; 3
  }|

@subsubsection[#:tag "iseries-range"]{iseries-range}
@defproc[#:link-target? #f
 (iseries-range [iseries ISeries] [pos Index])
(Vectorof Fixnum)]{
Returns the Fixnum value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-ISeries (vector 1 2 3 4)
  (build-index-from-labels (list 'a 'b 'c 'd))))

  (iseries-range series-integer 2) ; (vector 1 2)
  }|

@subsubsection[#:tag "iseries-length"]{iseries-length}
@defproc[#:link-target? #f
 (iseries-length [iseries ISeries])
Index]{
Returns the Fixnum value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-ISeries (vector 1 2 3 4)
  (build-index-from-labels (list 'a 'b 'c 'd))))

  (iseries-length series-integer) ; 4
  }|

(ISeries -> (Index -> Fixnum)
@subsubsection[#:tag "iseries-referencer"]{iseries-referencer}
@defproc[#:link-target? #f
 (iseries-referencer [iseries ISeries])
(Index -> Fixnum)]{
Returns the Fixnum value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-ISeries (vector 1 2 3 4)
  (build-index-from-labels (list 'a 'b 'c 'd))))

  ((iseries-referencer series-integer) 0) ; 1

  ((iseries-referencer series-integer) 1) ; 2
  }|


@subsubsection[#:tag "iseries-data"]{iseries-data}

@defproc[#:link-target? #f
 (iseries-data [iseries ISeries])
(Vectorof Fixnum)]{
Returns the Fixnum value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-ISeries (vector 1 2 3 4)
  (build-index-from-labels (list 'a 'b 'c 'd))))

  (iseries-data series-integer) ; (vector 1 2 3 4)
  }|

[map/is (ISeries (Fixnum -> Fixnum) -> ISeries)]
 [bop/is (ISeries ISeries (Fixnum Fixnum -> Fixnum) -> ISeries)]
 [comp/is (ISeries ISeries (Fixnum Fixnum -> Boolean) -> BSeries)]
@subsubsection[#:tag "map/is"]{map/is}

@defproc[#:link-target? #f
 (map/is [iseries ISeries] [fn (Fixnum -> Fixnum)])
ISeries]{
Returns the Fixnum value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-ISeries (vector 1 2 3 4)
  (build-index-from-labels (list 'a 'b 'c 'd))))

  (ISeries-data (map/is series-integer (λ: ((x : Fixnum)) (unsafe-fx+ x 1)))) ; (vector 2 3 4 5)
  }|

@subsubsection[#:tag "bop/is"]{bop/is}
(ISeries ISeries (Fixnum Fixnum -> Fixnum) -> ISeries)

@defproc[#:link-target? #f
 (bop/is [iseries ISeries] [iseries-2 ISeries] [fn (Fixnum Fixnum -> Fixnum)])
ISeries]{
Returns the Fixnum value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-ISeries (vector 1 2 3 4) #f))

  (define series-integer-2 (new-ISeries (vector 5 6 7 8) #f))

  (ISeries-data (bop/is series-integer series-integer-2 (λ: ((x : Fixnum) (y : Fixnum)) (unsafe-fx+ x y)))) ; (new-ISeries 6 8 10 12)
  }|

@subsubsection[#:tag "comp/is"]{comp/is}

@defproc[#:link-target? #f
 (comp/is [iseries ISeries] [iseries-2 ISeries] [fn (Fixnum Fixnum -> Boolean)])
BSeries]{
Returns the Fixnum value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-ISeries (vector 1 2 3 4) #f))

  (define series-integer-2 (new-ISeries (vector 5 6 7 8) #f))

  (ISeries-data (comp/is series-integer series-integer-2 (λ: ((x : Fixnum) (y : Fixnum)) (unsafe-fx> x y)))) ; (new-BSeries #f #f #f #f)
  }|


@subsubsection[#:tag "+/is"]{+/is}

@defproc[#:link-target? #f
 (+/is [iseries ISeries] [iseries-2 ISeries])
ISeries]{
Returns the Fixnum value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-ISeries (vector 1 2 3 4) #f))

  (define series-integer-2 (new-ISeries (vector 5 6 7 8) #f))

  (ISeries-data (+/is series-integer series-integer-2)) ; (vector 6 8 10 12)
  }|

@subsubsection[#:tag "-/is"]{-/is}

@defproc[#:link-target? #f
 (+/is [iseries ISeries] [iseries-2 ISeries])
ISeries]{
Returns the Fixnum value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-ISeries (vector 1 2 3 4) #f))

  (define series-integer-2 (new-ISeries (vector 5 6 7 8) #f))

  (ISeries-data (-/is series-integer series-integer-2)) ; (vector -4 -4 -4 -4)
  }|

@subsubsection[#:tag "*/is"]{*/is}

@defproc[#:link-target? #f
 (*/is [iseries ISeries] [iseries-2 ISeries])
ISeries]{
Returns the Fixnum value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-ISeries (vector 1 2 3 4) #f))

  (define series-integer-2 (new-ISeries (vector 5 6 7 8) #f))

  (ISeries-data (*/is series-integer series-integer-2)) ; (vector 5 12 21 32)
  }|

@subsubsection[#:tag "//is"]{//is}

@defproc[#:link-target? #f
 (//is [iseries ISeries] [iseries-2 ISeries])
ISeries]{
Returns the Fixnum value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-ISeries (vector 1 2 3 4) #f))

  (define series-integer-2 (new-ISeries (vector 5 6 7 8) #f))

  (ISeries-data (//is series-integer series-integer-2)) ; (vector 0 0 0 0)
  }|

@subsubsection[#:tag "iseries-data"]{%/is}

@defproc[#:link-target? #f
 (%/is [iseries ISeries] [iseries-2 ISeries])
ISeries]{
Returns the Fixnum value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-ISeries (vector 1 2 3 4) #f))

  (define series-integer-2 (new-ISeries (vector 5 6 7 8) #f))

  (ISeries-data (%/is series-integer series-integer-2)) ; (vector 1 2 3 4)
  }|

@subsubsection[#:tag "r/is"]{r/is}

@defproc[#:link-target? #f
 (r/is [iseries ISeries] [iseries-2 ISeries])
ISeries]{
Returns the Fixnum value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-ISeries (vector 1 2 3 4) #f))

  (define series-integer-2 (new-ISeries (vector 5 6 7 8) #f))

  (ISeries-data (r/is series-integer series-integer-2)) ; (vector 1 2 3 4)
  }|

@subsubsection[#:tag "+./is"]{+./is}

@defproc[#:link-target? #f
 (+./is [iseries ISeries] [num Fixnum])
ISeries]{
Returns the Fixnum value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-ISeries (vector 1 2 3 4) #f))

  (ISeries-data (+./is series-integer 2)) ; (vector 3 4 5 6)
  }|

@subsubsection[#:tag "-./is"]{-./is}

@defproc[#:link-target? #f
 (-./is [iseries ISeries] [num Fixnum])
ISeries]{
Returns the Fixnum value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-ISeries (vector 1 2 3 4) #f))

  (ISeries-data (-./is series-integer 2)) ; (vector -1 0 1 2)
  }|

@subsubsection[#:tag "*./is"]{*./is}

@defproc[#:link-target? #f
 (*./is [iseries ISeries] [num Fixnum])
ISeries]{
Returns the Fixnum value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-ISeries (vector 1 2 3 4) #f))

  (ISeries-data (*./is series-integer 2)) ; (vector 2 3 6 8)
  }|

@subsubsection[#:tag "/./is"]{/./is}

@defproc[#:link-target? #f
 (/./is [iseries ISeries] [num Fixnum])
ISeries]{
Returns the Fixnum value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-ISeries (vector 1 2 3 4) #f))

  (ISeries-data (/./is series-integer 2)) ; (vector 0 1 1 2)
  }|

@subsubsection[#:tag "%./is"]{%./is}

@defproc[#:link-target? #f
 (%./is [iseries ISeries] [num Fixnum])
ISeries]{
Returns the Fixnum value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-ISeries (vector 1 2 3 4) #f))

  (ISeries-data (%./is series-integer 2)) ; (vector 1 0 1 0)
  }|

@subsubsection[#:tag "r./is"]{r./is}

@defproc[#:link-target? #f
 (r./is [iseries ISeries] [num Fixnum])
ISeries]{
Returns the Fixnum value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-ISeries (vector 1 2 3 4) #f))

  (ISeries-data (r./is series-integer 2)) ; (vector 1 0 1 0)
  }|

@subsubsection[#:tag ">/is"]{>/is}

@defproc[#:link-target? #f
 (>/is [iseries ISeries] [iseries-2 ISeries])
BSeries]{
Returns the Fixnum value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-ISeries (vector 1 2 3 4) #f))

  (define series-integer-2 (new-ISeries (vector 5 6 7 8) #f))

  (BSeries-data (>/is series-integer series-integer-2)) ; (vector #f #f #f #f)
  }|

@subsubsection[#:tag "</is"]{</is}

@defproc[#:link-target? #f
 (</is [iseries ISeries] [iseries-2 ISeries])
BSeries]{
Returns the Fixnum value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-ISeries (vector 1 2 3 4) #f))

  (define series-integer-2 (new-ISeries (vector 5 6 7 8) #f))

  (BSeries-data (</is series-integer series-integer-2)) ; (vector #f #f #f #f)
  }|

@subsubsection[#:tag ">=/is"]{>=/is}

@defproc[#:link-target? #f
 (>=/is [iseries ISeries] [iseries-2 ISeries])
BSeries]{
Returns the Fixnum value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-ISeries (vector 1 2 3 4) #f))

  (define series-integer-2 (new-ISeries (vector 5 6 7 8) #f))

  (BSeries-data (>=/is series-integer series-integer-2)) ; (vector #f #f #f #f)
  }|

@subsubsection[#:tag "<=/is"]{<=/is}

@defproc[#:link-target? #f
 (<=/is [iseries ISeries] [iseries-2 ISeries])
BSeries]{
Returns the Fixnum value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-ISeries (vector 1 2 3 4) #f))

  (define series-integer-2 (new-ISeries (vector 5 6 7 8) #f))

  (BSeries-data (<=/is series-integer series-integer-2)) ; (vector #t #t #t #t)
  }|

@subsubsection[#:tag "=/is"]{=/is}

@defproc[#:link-target? #f
 (=/is [iseries ISeries] [iseries-2 ISeries])
BSeries]{
Returns the Fixnum value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-ISeries (vector 1 2 3 4) #f))

  (define series-integer-2 (new-ISeries (vector 5 6 7 8) #f))

  (BSeries-data (=/is series-integer series-integer-2)) ; (vector #f #f #f #f)
  }|

@subsubsection[#:tag "!=/is"]{!=/is}

@defproc[#:link-target? #f
 (!=/is [iseries ISeries] [iseries-2 ISeries])
BSeries]{
Returns the Fixnum value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-ISeries (vector 1 2 3 4) #f))

  (define series-integer-2 (new-ISeries (vector 5 6 7 8) #f))

  (BSeries-data (!=/is series-integer series-integer-2)) ; (vector #t #t #t #t)
  }|

@subsubsection[#:tag "{apply-agg-is"]{apply-agg-is}

@defproc[#:link-target? #f
 (apply-agg-is [func Symbol] [iseries ISeries])
Real]{
Returns the Fixnum value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-ISeries (vector 1 2 3 4) #f))

  (apply-agg-is 'sum series-integer) ; 10

  (apply-agg-is 'mean series-integer) ; 10/4

  (apply-agg-is 'count series-integer) ; 4

  (apply-agg-is 'min series-integer) ; 1

  (apply-agg-is 'max series-integer) ; 4
  }|

@subsubsection[#:tag "apply-stat-is"]{apply-stat-is}

@defproc[#:link-target? #f
 (apply-stat-is [func Symbol] [iseries ISeries])
Real]{
Returns the Fixnum value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-ISeries (vector 1 2 3 4) #f))

  (apply-stat-is 'variance series-integer) ; 5/4

  (apply-stat-is 'stddev series-integer) ; 1.118033988749895

  (apply-stat-is 'skewness series-integer) ; 0.0
  }|

@subsection[#:style 'toc]{Numerical Series}

@local-table-of-contents[]

@subsubsection[#:tag "new-NSeries"]{new-NSeries}
@defproc[#:link-target? #f
 (new-NSeries [data (Vectorof Float)] [label (Option (U (Lnstof Label) SIndex))])
NSeries?]{
Returns a new NSeries.
}

@codeblock|{
    (define series-float (new-NSeries (flvector 1.5 2.4 3.6 4.1)
                                      (build-index-from-labels (list 'a 'b 'c 'd))))
  }|

@subsubsection[#:tag "nseries-iref"]{nseries-iref}
@defproc[#:link-target? #f
 (nseries-iref [nseries NSeries] [idx Index])
Float?]{
Returns the Float value at the specified index in the series.
}

@codeblock|{
 
(define series-float (new-NSeries (flvector 1.5 2.4 3.6 4.1)
                                      (build-index-from-labels (list 'a 'b 'c 'd))))
                                      
(nseries-iref series-float 0) ; 1.5

(nseries-iref series-float 1) ; 2.4
  }|

@subsubsection[#:tag "nseries-label-ref"]{nseries-label-ref}
@defproc[#:link-target? #f
 (nseries-label-ref [nseries NSeries] [label Label])
Float?]{
Returns the Float value at the specified index in the series.
}

@codeblock|{
  (define series-float (new-NSeries (flvector 1.5 2.4 3.6 4.1)
                                      (build-index-from-labels (list 'a 'b 'c 'd))))

  (nseries-label-ref series-float 'd) ; 4.1
  
  (nseries-label-ref series-float 'c) ; 3.6
  }|

@subsubsection[#:tag "nseries-range"]{nseries-range}
@defproc[#:link-target? #f
 (nseries-range [nseries NSeries] [pos Index])
(Vectorof Float)]{
Returns the Float value at the specified index in the series.
}

@codeblock|{
  (define series-float (new-NSeries (flvector 1.5 2.4 3.6 4.1)
                                      (build-index-from-labels (list 'a 'b 'c 'd))))

  (nseries-range series-float 2) ; (flvector 1.5 2.4)
  }|

@subsubsection[#:tag "nseries-length"]{nseries-length}
@defproc[#:link-target? #f
 (nseries-length [nseries NSeries])
Index]{
Returns the Float value at the specified index in the series.
}

@codeblock|{
  (define series-float (new-NSeries (flvector 1.5 2.4 3.6 4.1)
  (build-index-from-labels (list 'a 'b 'c 'd))))

  (nseries-length series-float) ; 4
  }|

@subsubsection[#:tag "nseries-referencer"]{nseries-referencer}
@defproc[#:link-target? #f
 (nseries-referencer [nseries NSeries])
(Index -> Float)]{
Returns the Float value at the specified index in the series.
}

@codeblock|{
  (define series-float (new-NSeries (flvector 1.5 2.4 3.6 4.1)
  (build-index-from-labels (list 'a 'b 'c 'd))))

  ((nseries-referencer series-float) 0) ; 1.5

  ((nseries-referencer series-float) 1) ; 2.4
  }|


@subsubsection[#:tag "nseries-data"]{nseries-data}

@defproc[#:link-target? #f
 (nseries-data [nseries NSeries])
(Vectorof Float)]{
Returns the Float value at the specified index in the series.
}

@codeblock|{
  (define series-float (new-NSeries (flvector 1.5 2.4 3.6 4.1)
  (build-index-from-labels (list 'a 'b 'c 'd))))

  (nseries-data series-foat) ; (flvector 1.5 2.4 3.6 4.1)ap
  }|

@subsubsection[#:tag "map/ns"]{map/ns}

@defproc[#:link-target? #f
 (map/ns [nseries NSeries] [fn (Float -> Float)])
NSeries]{
Returns the Float value at the specified index in the series.
}

@codeblock|{
  (define series-float (new-NSeries (flvector 1.5 2.4 3.6 4.1)
  (build-index-from-labels (list 'a 'b 'c 'd))))

  (NSeries-data (map/ns series-float (λ: ((x : Float)) (fl+ x 1.0)))) ; (flvector 2.5 3.4 4.6 5.1)
  }|

@subsubsection[#:tag "bop/ns"]{bop/ns}
(NSeries NSeries (Float Float -> Float) -> NSeries)

@defproc[#:link-target? #f
 (bop/ns [nseries NSeries] [nseries-2 NSeries] [fn (Float Float -> Float)])
NSeries]{
Returns the Float value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-NSeries (vector 1 2 3 4) #f))

  (define series-integer-2 (new-NSeries (vector 5 6 7 8) #f))

  (NSeries-data (bop/ns series-integer series-integer-2 (λ: ((x : Float) (y : Float)) (unsafe-fx+ x y)))) ; (new-NSeries 6 8 10 12)
  }|

@subsubsection[#:tag "comp/ns"]{comp/ns}

@defproc[#:link-target? #f
 (comp/ns [nseries NSeries] [nseries-2 NSeries] [fn (Float Float -> Boolean)])
BSeries]{
Returns the Float value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-NSeries (vector 1 2 3 4) #f))

  (define series-integer-2 (new-NSeries (vector 5 6 7 8) #f))

  (NSeries-data (comp/ns series-integer series-integer-2 (λ: ((x : Float) (y : Float)) (unsafe-fx> x y)))) ; (new-BSeries #f #f #f #f)
  }|


@subsubsection[#:tag "+/ns"]{+/ns}

@defproc[#:link-target? #f
 (+/ns [nseries NSeries] [nseries-2 NSeries])
NSeries]{
Returns the Float value at the specified index in the series.
}

@codeblock|{
  (define series-float (new-NSeries (flvector 1.5 2.4 3.6 4.1)
                                      (build-index-from-labels (list 'a 'b 'c 'd))))

  (define series-float-2 (new-NSeries (flvector 5.0 6.0 7.0 8.0)
                                      (build-index-from-labels (list 'a 'b 'c 'd))))

  (NSeries-data (+/ns series-float series-float-2)) ; (flvector 6.5 8.4 10.6 12.1))
  }|

@subsubsection[#:tag "-/ns"]{-/ns}

@defproc[#:link-target? #f
 (+/ns [nseries NSeries] [nseries-2 NSeries])
NSeries]{
Returns the Float value at the specified index in the series.
}

@codeblock|{
  (define series-float (new-NSeries (flvector 1.5 2.4 3.6 4.1)
                                      (build-index-from-labels (list 'a 'b 'c 'd))))

  (define series-float-2 (new-NSeries (flvector 5.0 6.0 7.0 8.0)
                                      (build-index-from-labels (list 'a 'b 'c 'd))))

  (NSeries-data (-/ns series-float series-float-2)) ; (flvector -3.5 -3.6 -3.4 -3.9000000000000004)
  }|

@subsubsection[#:tag "*/ns"]{*/ns}

@defproc[#:link-target? #f
 (*/ns [nseries NSeries] [nseries-2 NSeries])
NSeries]{
Returns the Float value at the specified index in the series.
}

@codeblock|{
  (define series-float (new-NSeries (flvector 1.5 2.4 3.6 4.1)
                                      (build-index-from-labels (list 'a 'b 'c 'd))))

  (define series-float-2 (new-NSeries (flvector 5.0 6.0 7.0 8.0)
                                      (build-index-from-labels (list 'a 'b 'c 'd))))

  (NSeries-data (*/ns series-float series-float-2)) ; (flvector 7.5 14.399999999999999 25.2 32.8)
  }|

@subsubsection[#:tag "//ns"]{//ns}

@defproc[#:link-target? #f
 (//ns [nseries NSeries] [nseries-2 NSeries])
NSeries]{
Returns the Float value at the specified index in the series.
}

@codeblock|{
  (define series-float (new-NSeries (flvector 1.5 2.4 3.6 4.1)
                                      (build-index-from-labels (list 'a 'b 'c 'd))))

  (define series-float-2 (new-NSeries (flvector 5.0 6.0 7.0 8.0)
                                      (build-index-from-labels (list 'a 'b 'c 'd))))

  (NSeries-data (//ns series-float series-float-2)) ; (flvector 0.3 0.39999999999999997 0.5142857142857143 0.5125)
  }|

@subsubsection[#:tag "nseries-data"]{%/ns}

@defproc[#:link-target? #f
 (%/ns [nseries NSeries] [nseries-2 NSeries])
NSeries]{
Returns the Float value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-NSeries (vector 1 2 3 4) #f))

  (define series-integer-2 (new-NSeries (vector 5 6 7 8) #f))

  (NSeries-data (%/ns series-integer series-integer-2)) ; (vector 1 2 3 4)
  }|

@subsubsection[#:tag "r/ns"]{r/ns}

@defproc[#:link-target? #f
 (r/ns [nseries NSeries] [nseries-2 NSeries])
NSeries]{
Returns the Float value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-NSeries (vector 1 2 3 4) #f))

  (define series-integer-2 (new-NSeries (vector 5 6 7 8) #f))

  (NSeries-data (r/ns series-integer series-integer-2)) ; (vector 1 2 3 4)
  }|

@subsubsection[#:tag "+./ns"]{+./ns}

@defproc[#:link-target? #f
 (+./ns [nseries NSeries] [num Float])
NSeries]{
Returns the Float value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-NSeries (vector 1 2 3 4) #f))

  (NSeries-data (+./ns series-integer 2)) ; (vector 3 4 5 6)
  }|

@subsubsection[#:tag "-./ns"]{-./ns}

@defproc[#:link-target? #f
 (-./ns [nseries NSeries] [num Float])
NSeries]{
Returns the Float value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-NSeries (vector 1 2 3 4) #f))

  (NSeries-data (-./ns series-integer 2)) ; (vector -1 0 1 2)
  }|

@subsubsection[#:tag "*./ns"]{*./ns}

@defproc[#:link-target? #f
 (*./ns [nseries NSeries] [num Float])
NSeries]{
Returns the Float value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-NSeries (vector 1 2 3 4) #f))

  (NSeries-data (*./ns series-integer 2)) ; (vector 2 3 6 8)
  }|

@subsubsection[#:tag "/./ns"]{/./ns}

@defproc[#:link-target? #f
 (/./ns [nseries NSeries] [num Float])
NSeries]{
Returns the Float value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-NSeries (vector 1 2 3 4) #f))

  (NSeries-data (/./ns series-integer 2)) ; (vector 0 1 1 2)
  }|

@subsubsection[#:tag "%./ns"]{%./ns}

@defproc[#:link-target? #f
 (%./ns [nseries NSeries] [num Float])
NSeries]{
Returns the Float value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-NSeries (vector 1 2 3 4) #f))

  (NSeries-data (%./ns series-integer 2)) ; (vector 1 0 1 0)
  }|

@subsubsection[#:tag "r./ns"]{r./ns}

@defproc[#:link-target? #f
 (r./ns [nseries NSeries] [num Float])
NSeries]{
Returns the Float value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-NSeries (vector 1 2 3 4) #f))

  (NSeries-data (r./ns series-integer 2)) ; (vector 1 0 1 0)
  }|

@subsubsection[#:tag ">/ns"]{>/ns}

@defproc[#:link-target? #f
 (>/ns [nseries NSeries] [nseries-2 NSeries])
BSeries]{
Returns the Float value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-NSeries (vector 1 2 3 4) #f))

  (define series-integer-2 (new-NSeries (vector 5 6 7 8) #f))

  (BSeries-data (>/ns series-integer series-integer-2)) ; (vector #f #f #f #f)
  }|

@subsubsection[#:tag "</ns"]{</ns}

@defproc[#:link-target? #f
 (</ns [nseries NSeries] [nseries-2 NSeries])
BSeries]{
Returns the Float value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-NSeries (vector 1 2 3 4) #f))

  (define series-integer-2 (new-NSeries (vector 5 6 7 8) #f))

  (BSeries-data (</ns series-integer series-integer-2)) ; (vector #f #f #f #f)
  }|

@subsubsection[#:tag ">=/ns"]{>=/ns}

@defproc[#:link-target? #f
 (>=/ns [nseries NSeries] [nseries-2 NSeries])
BSeries]{
Returns the Float value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-NSeries (vector 1 2 3 4) #f))

  (define series-integer-2 (new-NSeries (vector 5 6 7 8) #f))

  (BSeries-data (>=/ns series-integer series-integer-2)) ; (vector #f #f #f #f)
  }|

@subsubsection[#:tag "<=/ns"]{<=/ns}

@defproc[#:link-target? #f
 (<=/ns [nseries NSeries] [nseries-2 NSeries])
BSeries]{
Returns the Float value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-NSeries (vector 1 2 3 4) #f))

  (define series-integer-2 (new-NSeries (vector 5 6 7 8) #f))

  (BSeries-data (<=/ns series-integer series-integer-2)) ; (vector #t #t #t #t)
  }|

@subsubsection[#:tag "=/ns"]{=/ns}

@defproc[#:link-target? #f
 (=/ns [nseries NSeries] [nseries-2 NSeries])
BSeries]{
Returns the Float value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-NSeries (vector 1 2 3 4) #f))

  (define series-integer-2 (new-NSeries (vector 5 6 7 8) #f))

  (BSeries-data (=/ns series-integer series-integer-2)) ; (vector #f #f #f #f)
  }|

@subsubsection[#:tag "!=/ns"]{!=/ns}

@defproc[#:link-target? #f
 (!=/ns [nseries NSeries] [nseries-2 NSeries])
BSeries]{
Returns the Float value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-NSeries (vector 1 2 3 4) #f))

  (define series-integer-2 (new-NSeries (vector 5 6 7 8) #f))

  (BSeries-data (!=/ns series-integer series-integer-2)) ; (vector #t #t #t #t)
  }|

@subsubsection[#:tag "{apply-agg-ns"]{apply-agg-ns}

@defproc[#:link-target? #f
 (apply-agg-ns [func Symbol] [nseries NSeries])
Real]{
Returns the Float value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-NSeries (vector 1 2 3 4) #f))

  (apply-agg-ns 'sum series-integer) ; 10

  (apply-agg-ns 'mean series-integer) ; 10/4

  (apply-agg-ns 'count series-integer) ; 4

  (apply-agg-ns 'min series-integer) ; 1

  (apply-agg-ns 'max series-integer) ; 4
  }|

@subsubsection[#:tag "apply-stat-ns"]{apply-stat-ns}

@defproc[#:link-target? #f
 (apply-stat-ns [func Symbol] [nseries NSeries])
Real]{
Returns the Float value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-NSeries (vector 1 2 3 4) #f))

  (apply-stat-ns 'variance series-integer) ; 5/4

  (apply-stat-ns 'stddev series-integer) ; 1.118033988749895

  (apply-stat-ns 'skewness series-integer) ; 0.0
  }|


@subsection[#:style 'toc]{Categorical Series}

@local-table-of-contents[]

@subsubsection[#:tag "new-CSeries"]{new-CSeries}

@"\U2190" This page has no on-this-page panel in a multi-page
rendering, because there are no numbered subsections, but it has three
levels shown in the table-of-contents panel.

@subsection[#:style 'toc]{Boolean Series}

@local-table-of-contents[]

@subsubsection[#:tag "new-BSeries"]{new-BSeries}

@"\U2190" This page has no on-this-page panel in a multi-page
rendering, because there are no numbered subsections, but it has three
levels shown in the table-of-contents panel.


@subsection[#:style 'toc]{General Series}

@local-table-of-contents[]

@subsubsection[#:tag "new-GSeries"]{new-GSeries}

@"\U2190" This page has no on-this-page panel in a multi-page
rendering, because there are no numbered subsections, but it has three
levels shown in the table-of-contents panel.


@; ======================================================================

@section[#:tag "Indexing"]{Indexing}

@itemlist[

 @item{
The axis labeling information in RacketFrame objects serves many purposes:

Identifies data (i.e. provides metadata) using known indicators, important for analysis, visualization, and interactive console display.
Enables automatic and explicit data alignment.
Allows intuitive getting and setting of subsets of the data set.
In this section, we will focus on the final point: namely, how to slice, dice, and generally get and set subsets of pandas objects. The primary focus will be on Series and DataFrame as they have received more development attention in this area.
}

]

@subsection[#:style 'toc]{Index}

@subsubsection[#:tag "data-frame-loc"]{data-frame-loc}

@"\U2190" data-frame-loc is primarily label based, but may also be used with a boolean array. data-frame-loc will raise KeyError when the items are not found. Allowed inputs are:

A single label, e.g. 5 or 'a' (Note that 5 is interpreted as a label of the index. This use is not an integer position along the index.).

A list or array of labels ['a', 'b', 'c'].

A slice object with labels 'a':'f' (Note that contrary to usual python slices, both the start and the stop are included, when present in the index! See Slicing with labels.).

A boolean array.

A callable function with one argument (the calling Series, DataFrame or Panel) and that returns valid output for indexing (one of the above).

@subsubsection[#:tag "data-frame-iloc"]{data-frame-iloc}

data-frame-iloc is primarily integer position based (from 0 to length-1 of the axis), but may also be used with a boolean array. data-frame-iloc will raise IndexError if a requested indexer is out-of-bounds, except slice indexers which allow out-of-bounds indexing. (this conforms with Python/NumPy slice semantics). Allowed inputs are:

An integer e.g. 5.

A list or array of integers [4, 3, 0].

A slice object with ints 1:7.

A boolean array.

A callable function with one argument (the calling Series, DataFrame or Panel) and that returns valid output for indexing (one of the above).

@subsection[#:style 'toc]{MultiIndex}

@subsection[#:style 'toc]{DateTimeIndex}

@; ======================================================================
@section{DataFrames}

@itemlist[

 @item{One-dimensional ndarray with axis labels (including time series).

Labels need not be unique but must be a hashable type. The object supports both integer- and label-based indexing and provides a host of methods for performing operations involving the index. Statistical methods from ndarray have been overridden to automatically exclude missing data (currently represented as NaN).

Operations between Series (+, -, /, , *) align values based on their associated index values– they need not be the same length. The result index will be the sorted union of the two indexes.
}

]

@subsection[#:style 'toc]{DataFrame}

@local-table-of-contents[]

@subsubsection[#:tag "new-data-frame"]{new-data-frame}
@defproc[#:link-target? #f
 (new-data-frame [columns Columns])
DataFrame?]{
Returns a new DataFrame.
}

@codeblock|{
   ;******************
  ;data-frame-integer
  ;******************
  ; will define parse to automatically build this columns structure
  (define columns-integer
  (list 
  (cons 'col1 (new-ISeries (vector 1 2 3 4)
  (build-index-from-labels (list 'a 'b 'c 'd))))
  (cons 'col2 (new-ISeries (vector 5 6 7 8)
  (build-index-from-labels (list 'e 'f 'g 'h))))
  (cons 'col3 (new-ISeries (vector 9 10 11 12)
  (build-index-from-labels (list 'i 'j 'k 'l))))))

  ; create new data-frame-integer
  (define data-frame-integer (new-data-frame columns-integer))
  }|

@subsubsection[#:tag "data-frame-rename"]{data-frame-rename}
@defproc[#:link-target? #f
 (data-frame-rename [df DataFrame] [col Label] [new-col Label])
DataFrame?]{
 This function consumes a DataFrame and two labels. It locates
 the index of the column in the DataFrame LabelIndex and creates
 a new LabelIndex with the new column name mapped to this index.
 Then a new DataFrame is created and returned.
}

@codeblock|{
    (data-frame-rename)
  }|

@subsubsection[#:tag "data-frame-drop"]{data-frame-drop}
@defproc[#:link-target? #f
 (data-frame-drop [df DataFrame] [col-name Label])
DataFrame?]{
 This function consumes a DataFrame and a Label for the column
 to drop from the DataFrame. Using the data-frame-explode
 function, this column is filtered out of the DataFrame, and a
 new DataFrame is returned.
}

@codeblock|{
    (data-frame-drop)
  }|

@subsubsection[#:tag "data-frame-series"]{data-frame-series}
@defproc[#:link-target? #f
 (data-frame-drop [df DataFrame] [col-name Label])
Series?]{
 This function consumes a DataFrame and a Symbol representing
 the column name and returns the Series of that column.
}

@codeblock|{
    (data-frame-series)
  }|

@subsubsection[#:tag "data-frame-names"]{data-frame-names}
@defproc[#:link-target? #f
 (data-frame-names [df DataFrame] [col-name Label])
Series?]{
 This function consumes a DataFrame and returns of Listof
 Symbol representing all the column names.
}

@codeblock|{
    (data-frame-names)
  }|

@subsubsection[#:tag "data-frame-dim"]{data-frame-dim}
@defproc[#:link-target? #f
 (data-frame-dim [df DataFrame])
Dim?]{
 This function consumes a DataFrame and calculates the dimensions.
 It does so by calculating the length of a column series, thus
 getting the number of rows and the total number of columns. It
 then stores these two values in the Dim struct and returns it.
}

@codeblock|{
    (data-frame-dim)
  }|


@subsubsection[#:tag "data-frame-description"]{data-frame-description}
@defproc[#:link-target? #f
 (data-frame-description [df DataFrame] [project LabelProjection])
DataFrameDescription?]{
 This function consumes a DataFrame and an optional LabelProjection
 and constructs a DataFrameDescription struct which contains the
 DataFrame Dim and a list of SeriesDecriptions for each series of
 each column.
}

@codeblock|{
    (data-frame-description)
  }|

@subsubsection[#:tag "show-data-frame-description"]{show-data-frame-description}
@defproc[#:link-target? #f
 (show-data-frame-description [dfd DataFrameDescription])
Void?]{
 This function consumes a DataFrameDescription struct and displays
 the DataFrame Dim and Series Descriptions in formated form.
}

@codeblock|{
    (show-data-frame-description)
  }|

@subsubsection[#:tag "data-frame-explode"]{data-frame-explode}
@defproc[#:link-target? #f
 (data-frame-explode [df DataFrame] [project LabelProjection])
Columns?]{
 This function consumes a DataFrame and an optional LabelProjection
 which indicates which columns to project and returns a Listof Column.
 If no LabelProjection is given, all columns are projected. The
 label-sort-positional returns a labeling, which is a (Listof (Pair Label Index)).
 This list is looped through and only the items to project are filtered for.
 In the end an appropriate Listof Column is returned.
}

@codeblock|{
    (data-frame-explode)
  }|

@subsubsection[#:tag "data-frame-remove"]{data-frame-remove}
@defproc[#:link-target? #f
 (data-frame-remove [df DataFrame] [project LabelProjection])
DataFrame?]{
 This function consumes a DataFrame and LabelProjection indicating
 which columns to remove from the DataFrame. The column names in
 the DataFrame are put into a set called all-labels. The drop-projection
 is converted to a set as well. The labels to be kept are calculated
 by substracting the drop-projection from all of the possible labels.
 Then the DataFrame is exploded retaining only the columns to keep.
 A new data frame is constructed fomr these column and returned.
}

@codeblock|{
    (data-frame-remove)
  }|

@subsubsection[#:tag "data-frame-project"]{data-frame-project}
@defproc[#:link-target? #f
 (data-frame-project [df DataFrame] [project LabelProjection])
DataFrame?]{
 This function consumes a DataFrame and LabelProjection and
 creates a new DataFrame only containing the columns to project.
}

@codeblock|{
    (data-frame-project)
  }|

@subsubsection[#:tag "data-frame-replace"]{data-frame-replace}
@defproc[#:link-target? #f
 (data-frame-replace [df DataFrame] [col Column])
DataFrame?]{
 This function consumes a DataFrame and Column and replaces
 the old column with the same column heading with this new
 column.
}

@codeblock|{
    (data-frame-replace)
  }|

@subsubsection[#:tag "data-frame-extend"]{data-frame-extend}
@defproc[#:link-target? #f
 (data-frame-extend [df DataFrame] [col (U Column Columns DataFrame)])
DataFrame?]{
 This functions consumes a DataFrame and either a Column,
 Listof Column or a DataFrame and extends the given DataFrame.
 If a DataFrame is passed for the second argument it is first
 exploded and appended on to the exploded given data frame. Then
 a new DataFrame is constructed and returned. The other cases
 are self-explanatory.
}

@codeblock|{
    (data-frame-extend)
  }|

@subsection[#:style 'toc]{DataFrame Operations}

@local-table-of-contents[]

@subsubsection[#:tag "data-frame+"]{data-frame+}
@defproc[#:link-target? #f
 (data-frame+ [dfa DataFrame] [dfb DataFrame])
DataFrame?]{
Returns a new DataFrame.
}

@codeblock|{
  (define columns-integer-1
  (list 
  (cons 'col1 (new-ISeries (vector 1 2 3 4) #f))
  (cons 'col2 (new-ISeries (vector 5 6 7 8) #f))
  (cons 'col3 (new-ISeries (vector 9 10 11 12) #f))
  (cons 'col4 (new-ISeries (vector 21 22 23 24) #f))))

  (define columns-integer-2
  (list 
  (cons 'col1 (new-ISeries (vector 1 2 3 4) #f))
  (cons 'col2 (new-ISeries (vector 25 26 27 28) #f))
  (cons 'col3 (new-ISeries (vector 29 30 31 32) #f))
  (cons 'col4 (new-ISeries (vector 1 2 3 4) #f))))

  ; create new data-frame-integer-1
  (define data-frame-integer-1 (new-data-frame columns-integer-1))

  ; create new data-frame-integer-2
  (define data-frame-integer-2 (new-data-frame columns-integer-2))

  (displayln "data-frame+")

  (frame-write-tab data-frame-integer-1 (current-output-port))

  (frame-write-tab data-frame-integer-2 (current-output-port))

  (frame-write-tab (data-frame+ data-frame-integer-1 data-frame-integer-2) (current-output-port))
  }|

@subsubsection[#:tag "data-frame-"]{data-frame-}
@defproc[#:link-target? #f
 (data-frame- [dfa DataFrame] [dfb DataFrame])
DataFrame?]{
Returns a new DataFrame.
}

@codeblock|{
  (define columns-integer-1
  (list 
  (cons 'col1 (new-ISeries (vector 1 2 3 4) #f))
  (cons 'col2 (new-ISeries (vector 5 6 7 8) #f))
  (cons 'col3 (new-ISeries (vector 9 10 11 12) #f))
  (cons 'col4 (new-ISeries (vector 21 22 23 24) #f))))

  (define columns-integer-2
  (list 
  (cons 'col1 (new-ISeries (vector 1 2 3 4) #f))
  (cons 'col2 (new-ISeries (vector 25 26 27 28) #f))
  (cons 'col3 (new-ISeries (vector 29 30 31 32) #f))
  (cons 'col4 (new-ISeries (vector 1 2 3 4) #f))))

  ; create new data-frame-integer-1
  (define data-frame-integer-1 (new-data-frame columns-integer-1))

  ; create new data-frame-integer-2
  (define data-frame-integer-2 (new-data-frame columns-integer-2))

  (displayln "data-frame-")

  (frame-write-tab data-frame-integer-1 (current-output-port))

  (frame-write-tab data-frame-integer-2 (current-output-port))

  (frame-write-tab (data-frame- data-frame-integer-1 data-frame-integer-2) (current-output-port))
  }|

@subsubsection[#:tag "data-frame*"]{data-frame*}
@defproc[#:link-target? #f
 (data-frame* [dfa DataFrame] [dfb DataFrame])
DataFrame?]{
Returns a new DataFrame.
}

@codeblock|{
  (define columns-integer-1
  (list 
  (cons 'col1 (new-ISeries (vector 1 2 3 4) #f))
  (cons 'col2 (new-ISeries (vector 5 6 7 8) #f))
  (cons 'col3 (new-ISeries (vector 9 10 11 12) #f))
  (cons 'col4 (new-ISeries (vector 21 22 23 24) #f))))

  (define columns-integer-2
  (list 
  (cons 'col1 (new-ISeries (vector 1 2 3 4) #f))
  (cons 'col2 (new-ISeries (vector 25 26 27 28) #f))
  (cons 'col3 (new-ISeries (vector 29 30 31 32) #f))
  (cons 'col4 (new-ISeries (vector 1 2 3 4) #f))))

  ; create new data-frame-integer-1
  (define data-frame-integer-1 (new-data-frame columns-integer-1))

  ; create new data-frame-integer-2
  (define data-frame-integer-2 (new-data-frame columns-integer-2))

  (displayln "data-frame*")

  (frame-write-tab data-frame-integer-1 (current-output-port))

  (frame-write-tab data-frame-integer-2 (current-output-port))

  (frame-write-tab (data-frame* data-frame-integer-1 data-frame-integer-2) (current-output-port))
  }|

@subsubsection[#:tag "data-frame/"]{data-frame/}
@defproc[#:link-target? #f
 (data-frame/ [dfa DataFrame] [dfb DataFrame])
DataFrame?]{
Returns a new DataFrame.
}

@codeblock|{
  (define columns-integer-1
  (list 
  (cons 'col1 (new-ISeries (vector 1 2 3 4) #f))
  (cons 'col2 (new-ISeries (vector 5 6 7 8) #f))
  (cons 'col3 (new-ISeries (vector 9 10 11 12) #f))
  (cons 'col4 (new-ISeries (vector 21 22 23 24) #f))))

  (define columns-integer-2
  (list 
  (cons 'col1 (new-ISeries (vector 1 2 3 4) #f))
  (cons 'col2 (new-ISeries (vector 25 26 27 28) #f))
  (cons 'col3 (new-ISeries (vector 29 30 31 32) #f))
  (cons 'col4 (new-ISeries (vector 1 2 3 4) #f))))

  ; create new data-frame-integer-1
  (define data-frame-integer-1 (new-data-frame columns-integer-1))

  ; create new data-frame-integer-2
  (define data-frame-integer-2 (new-data-frame columns-integer-2))

  (displayln "data-frame/")

  (frame-write-tab data-frame-integer-1 (current-output-port))

  (frame-write-tab data-frame-integer-2 (current-output-port))

  (frame-write-tab (data-frame/ data-frame-integer-1 data-frame-integer-2) (current-output-port))
  }|

@subsubsection[#:tag "data-frame%"]{data-frame%}
@defproc[#:link-target? #f
 (data-frame% [dfa DataFrame] [dfb DataFrame])
DataFrame?]{
Returns a new DataFrame.
}

@subsubsection[#:tag "data-frame-r"]{data-frame-r}
@defproc[#:link-target? #f
 (data-frame-r [dfa DataFrame] [dfb DataFrame])
DataFrame?]{
Returns a new DataFrame.
}

@subsubsection[#:tag "data-frame="]{data-frame=}
@defproc[#:link-target? #f
 (data-frame= [dfa DataFrame] [dfb DataFrame])
DataFrame?]{
Returns a new DataFrame.
}

@subsubsection[#:tag "data-frame!="]{data-frame!=}
@defproc[#:link-target? #f
 (data-frame!= [dfa DataFrame] [dfb DataFrame])
DataFrame?]{
Returns a new DataFrame.
}

@subsubsection[#:tag "data-frame<"]{data-frame<}
@defproc[#:link-target? #f
 (data-frame< [dfa DataFrame] [dfb DataFrame])
DataFrame?]{
Returns a new DataFrame.
}

@subsubsection[#:tag "data-frame>"]{data-frame>}
@defproc[#:link-target? #f
 (data-frame> [dfa DataFrame] [dfb DataFrame])
DataFrame?]{
Returns a new DataFrame.
}

@subsubsection[#:tag "data-frame<="]{data-frame<=}
@defproc[#:link-target? #f
 (data-frame<= [dfa DataFrame] [dfb DataFrame])
DataFrame?]{
Returns a new DataFrame.
}

@subsubsection[#:tag "data-frame>="]{data-frame>=}
@defproc[#:link-target? #f
 (data-frame>= [dfa DataFrame] [dfb DataFrame])
DataFrame?]{
Returns a new DataFrame.
}

@subsubsection[#:tag "data-frame-abs"]{data-frame-abs}
@defproc[#:link-target? #f
 (data-frame-abs [df DataFrame])
DataFrame?]{
Returns a new DataFrame.
}

@subsection[#:style 'toc]{DataFrame Join}

@local-table-of-contents[]

@subsubsection[#:tag "data-frame-join-left"]{data-frame-join-left}
@defproc[#:link-target? #f
 (data-frame-join-left [dfa DataFrame] [dfb DataFrame] [on (Listof Symbol)])
DataFrame?]{
Returns a new DataFrame.
}

@codeblock|{
   (define columns-mixed-7
  (list 
   (cons 'col1 (new-ISeries (vector 1 2 3 4) #f))
   (cons 'col2 (new-CSeries (vector 'a 'b 'c 'd)))
   (cons 'col3 (new-GenSeries (vector 'a 1.5 20 10) #f))
   (cons 'col4 (new-ISeries (vector 21 22 23 24) #f))))

(define columns-mixed-8
  (list 
   (cons 'col1 (new-ISeries (vector 11 21 31 41) #f))
   (cons 'col2 (new-CSeries (vector 'a 'b 'g 'd)))
   (cons 'col3 (new-GenSeries (vector 'a 'b 'c 'd) #f))
   (cons 'col4 (new-ISeries (vector 22 22 23 24) #f))))

; create new data-frame-mixed-7
(define data-frame-mixed-7 (new-data-frame columns-mixed-7))

; create new data-frame-mixed-8
(define data-frame-mixed-8 (new-data-frame columns-mixed-8))

(frame-write-tab data-frame-mixed-7 (current-output-port))

(frame-write-tab data-frame-mixed-8 (current-output-port))

(frame-write-tab (data-frame-join-left data-frame-mixed-7 data-frame-mixed-8
  #:on (list 'col3)) (current-output-port))

;; output ;;
col1	col2	col3	col4
1	a	a	21
2	b	1.5	22
3	c	20	23
4	d	10	24

col1	col2	col3	col4
11	a	a	22
21	b	b	22
31	g	c	23
41	d	d	24

dfa-col1	dfa-col2	dfa-col3	dfa-col4	dfb-col1	dfb-col2	dfb-col4
1		a		a		21		11		a		22
2		b		1.5		22		0		null		0	
3		c		20		23		0		null		0
4		d		10		24		0		null		0
;; output ;;

  }|

@subsubsection[#:tag "data-frame-join-right"]{data-frame-join-right}
@defproc[#:link-target? #f
 (data-frame-join-right [dfa DataFrame] [dfb DataFrame] [on (Listof Symbol)])
DataFrame?]{
Returns a new DataFrame.
}

@codeblock|{
   (define columns-mixed-7
  (list 
   (cons 'col1 (new-ISeries (vector 1 2 3 4) #f))
   (cons 'col2 (new-CSeries (vector 'a 'b 'c 'd)))
   (cons 'col3 (new-GenSeries (vector 'a 1.5 20 10) #f))
   (cons 'col4 (new-ISeries (vector 21 22 23 24) #f))))

(define columns-mixed-8
  (list 
   (cons 'col1 (new-ISeries (vector 11 21 31 41) #f))
   (cons 'col2 (new-CSeries (vector 'a 'b 'g 'd)))
   (cons 'col3 (new-GenSeries (vector 'a 'b 'c 'd) #f))
   (cons 'col4 (new-ISeries (vector 22 22 23 24) #f))))

; create new data-frame-mixed-7
(define data-frame-mixed-7 (new-data-frame columns-mixed-7))

; create new data-frame-mixed-8
(define data-frame-mixed-8 (new-data-frame columns-mixed-8))

(frame-write-tab data-frame-mixed-7 (current-output-port))

(frame-write-tab data-frame-mixed-8 (current-output-port))

(frame-write-tab (data-frame-join-right data-frame-mixed-7 data-frame-mixed-8
  #:on (list 'col3)) (current-output-port))

;; output ;;
col1	col2	col3	col4
1	a	a	21
2	b	1.5	22
3	c	20	23
4	d	10	24

  col1	col2	col3	col4
11	a	a	22
21	b	b	22
31	g	c	23
41	d	d	24

dfa-col1	dfa-col2	dfa-col4	dfb-col1	dfb-col2	dfb-col3	dfb-col4
1		a		21		11		a		a		22
0		null		0		21		b		b		22
0		null		0		31		g		c		23
0		null		0		41		d		d		24
;; output ;;
  }|

@subsubsection[#:tag "data-frame-join-inner"]{data-frame-join-inner}
@defproc[#:link-target? #f
 (data-frame-join-inner [dfa DataFrame] [dfb DataFrame] [on (Listof Symbol)])
DataFrame?]{
Returns a new DataFrame.
}

@codeblock|{
   (define columns-mixed-7
  (list 
   (cons 'col1 (new-ISeries (vector 1 2 3 4) #f))
   (cons 'col2 (new-CSeries (vector 'a 'b 'c 'd)))
   (cons 'col3 (new-GenSeries (vector 'a 1.5 20 10) #f))
   (cons 'col4 (new-ISeries (vector 21 22 23 24) #f))))

(define columns-mixed-8
  (list 
   (cons 'col1 (new-ISeries (vector 11 21 31 41) #f))
   (cons 'col2 (new-CSeries (vector 'a 'b 'g 'd)))
   (cons 'col3 (new-GenSeries (vector 'a 'b 'c 'd) #f))
   (cons 'col4 (new-ISeries (vector 22 22 23 24) #f))))

; create new data-frame-mixed-7
(define data-frame-mixed-7 (new-data-frame columns-mixed-7))

; create new data-frame-mixed-8
(define data-frame-mixed-8 (new-data-frame columns-mixed-8))

(frame-write-tab data-frame-mixed-7 (current-output-port))

(frame-write-tab data-frame-mixed-8 (current-output-port))

(frame-write-tab (data-frame-join-inner data-frame-mixed-7 data-frame-mixed-8
  #:on (list 'col3)) (current-output-port))

;; output ;;
col1	col2	col3	col4
1	a	a	21
2	b	1.5	22
3	c	20	23
4	d	10	24

  col1	col2	col3	col4
11	a	a	22
21	b	b	22
31	g	c	23
41	d	d	24

dfa-col1	dfa-col2	dfa-col3	dfa-col4	dfb-col1	dfb-col2	dfb-col3	dfb-col4
1		a		a		21		11		a		a		22
;; output ;;
  }|

@subsubsection[#:tag "data-frame-join-outer"]{data-frame-join-outer}
@defproc[#:link-target? #f
 (data-frame-join-outer [dfa DataFrame] [dfb DataFrame] [on (Listof Symbol)])
DataFrame?]{
Returns a new DataFrame.
}

@codeblock|{
(define columns-mixed-7
  (list 
   (cons 'col1 (new-ISeries (vector 1 2 3 4) #f))
   (cons 'col2 (new-CSeries (vector 'a 'b 'c 'd)))
   (cons 'col3 (new-GenSeries (vector 'a 1.5 20 10) #f))
   (cons 'col4 (new-ISeries (vector 21 22 23 24) #f))))

(define columns-mixed-8
  (list 
   (cons 'col1 (new-ISeries (vector 11 21 31 41) #f))
   (cons 'col2 (new-CSeries (vector 'a 'b 'g 'd)))
   (cons 'col3 (new-GenSeries (vector 'a 'b 'c 'd) #f))
   (cons 'col4 (new-ISeries (vector 22 22 23 24) #f))))

; create new data-frame-mixed-7
(define data-frame-mixed-7 (new-data-frame columns-mixed-7))

; create new data-frame-mixed-8
(define data-frame-mixed-8 (new-data-frame columns-mixed-8))

(frame-write-tab data-frame-mixed-7 (current-output-port))

(frame-write-tab data-frame-mixed-8 (current-output-port))

(frame-write-tab (data-frame-join-outer data-frame-mixed-7 data-frame-mixed-8
  #:on (list 'col3)) (current-output-port))

;; output ;;
col1	col2	col3	col4
1	a	a	21
2	b	1.5	22
3	c	20	23
4	d	10	24

col1	col2	col3	col4
11	a	a	22
21	b	b	22
31	g	c	23
41	d	d	24

dfa-col1	dfa-col2	dfa-col3	dfa-col4	dfb-col1	dfb-col2	dfb-col3	dfb-col4
1		a		a		21		11		a		a		22
2		b		1.5		22		0		null		null		0
3		c		20		23		0		null		null		0
4		d		10		24		0		null		null		0
0		null		null		0		21		b		b		22
0		null		null		0		31		g		c		23
0		null		null		0		41		d		d		24
;; output ;;
  }|

@subsubsection[#:tag "data-frame-concat-vertical"]{data-frame-concat-vertical}
@defproc[#:link-target? #f
 (data-frame-concat-vertical [dfa DataFrame] [dfb DataFrame] [col (Listof Symbol)])
DataFrame?]{
Returns a new DataFrame.
}

@codeblock|{
(define columns-mixed-1
  (list 
   (cons 'col1 (new-ISeries (vector 1 2 3 4) #f))
   (cons 'col2 (new-NSeries (flvector 5.2 6.2 7.2 8.2) #f))
   (cons 'col3 (new-CSeries (vector 'a 'b 'c 'd)))
   (cons 'col4 (new-ISeries (vector 21 22 23 24) #f))))

(define columns-mixed-2
  (list 
   (cons 'col1 (new-ISeries (vector 1 2 3 4) #f))
   (cons 'col2 (new-NSeries (flvector 25.3 26.3 27.3 28.3) #f))
   (cons 'col3 (new-CSeries (vector 'e 'f 'g 'h)))
   (cons 'col4 (new-ISeries (vector 1 2 3 4) #f))))

; create new data-frame-mixed-1
(define data-frame-mixed-1 (new-data-frame columns-mixed-1))

; create new data-frame-mixed-2
(define data-frame-mixed-2 (new-data-frame columns-mixed-2))

; display created data frames
(frame-write-tab data-frame-mixed-1 (current-output-port))

(frame-write-tab data-frame-mixed-2 (current-output-port))

; vertical concat
(frame-write-tab (data-frame-concat-vertical data-frame-mixed-1 data-frame-mixed-2) (current-output-port))

;; output ;;
col1	col2	col3	col4
1	5.2	a	21
2	6.2	b	22
3	7.2	c	23
4	8.2	d	24

  col1	col2	col3	col4
1	25.3	e	1
2	26.3	f	2
3	27.3	g	3
4	28.3	h	4

col1	col2	col3	col4
1	5.2	a	21
2	6.2	b	22
3	7.2	c	23
4	8.2	d	24
1	25.3	e	1
2	26.3	f	2
3	27.3	g	3
4	28.3	h	4
;; output ;;
  }|

@subsubsection[#:tag "data-frame-concat-horizontal"]{data-frame-concat-horizontal}
@defproc[#:link-target? #f
 (data-frame-concat-horizontal [dfa DataFrame] [dfb DataFrame] [col (Listof Symbol)])
DataFrame?]{
Returns a new DataFrame.
}

@codeblock|{
(define columns-mixed-1
  (list 
   (cons 'col1 (new-ISeries (vector 1 2 3 4) #f))
   (cons 'col2 (new-NSeries (flvector 5.2 6.2 7.2 8.2) #f))
   (cons 'col3 (new-CSeries (vector 'a 'b 'c 'd)))
   (cons 'col4 (new-ISeries (vector 21 22 23 24) #f))))

(define columns-mixed-2
  (list 
   (cons 'col1 (new-ISeries (vector 1 2 3 4) #f))
   (cons 'col2 (new-NSeries (flvector 25.3 26.3 27.3 28.3) #f))
   (cons 'col3 (new-CSeries (vector 'e 'f 'g 'h)))
   (cons 'col4 (new-ISeries (vector 1 2 3 4) #f))))

; create new data-frame-mixed-1
(define data-frame-mixed-1 (new-data-frame columns-mixed-1))

; create new data-frame-mixed-2
(define data-frame-mixed-2 (new-data-frame columns-mixed-2))

; display created data frames
(frame-write-tab data-frame-mixed-1 (current-output-port))

(frame-write-tab data-frame-mixed-2 (current-output-port))

; horizontal concat
(frame-write-tab (data-frame-concat-horizontal data-frame-mixed-1 data-frame-mixed-2) (current-output-port))

;; output ;;
col1	col2	col3	col4
1	5.2	a	21
2	6.2	b	22
3	7.2	c	23
4	8.2	d	24

col1	col2	col3	col4
1	25.3	e	1
2	26.3	f	2
3	27.3	g	3
4	28.3	h	4

dfa-col1	dfa-col2	dfa-col3	dfa-col4	dfb-col1	dfb-col2	dfb-col3	dfb-col4
1	        5.2		a		21		1		25.3	        e		1
2	        6.2		b		22		2		26.3	        f		2
3	        7.2		c		23		3		27.3	        g		3
4  		8.2		d		24		4		28.3	        h		4   
;; output ;;
  }|

@subsubsection[#:tag "data-frame-groupby"]{data-frame-groupby}
@defproc[#:link-target? #f
 (data-frame-groupby [dfa DataFrame] [by (Listof Symbol)])
GroupHash?]{
Returns a new DataFrame.
}

@codeblock|{
(define columns-integer
  (list 
   (cons 'col1 (new-ISeries (vector 1 2 3 4 2 ) #f))
   (cons 'col2 (new-ISeries (vector 5 6 7 8 6) #f))
   (cons 'col3 (new-ISeries (vector 9 10 11 12 17) #f))
   (cons 'col4 (new-ISeries (vector 13 14 15 16 18) #f))))

; create new data-frame-integer
(define data-frame-integer (new-data-frame columns-integer))

(frame-write-tab data-frame-integer (current-output-port))

(data-frame-groupby data-frame-integer (list 'col1))

(data-frame-groupby data-frame-integer (list 'col2))

(data-frame-groupby data-frame-integer (list 'col1 'col2))
            
;; output ;;

col1	col2	col3	col4
1	5	9	13
2	6	10	14
3	7	11	15
4	8	12	16
2	6	17	18

'#hash(("2\t" . (6 17 18 6 10 14))
       ("1\t" . (5 9 13))
       ("4\t" . (8 12 16))
       ("3\t" . (7 11 15)))
       
'#hash(("8\t" . (4 12 16))
       ("7\t" . (3 11 15))
       ("6\t" . (2 17 18 2 10 14))
       ("5\t" . (1 9 13)))
       
'#hash(("4\t8\t" . (12 16))
       ("2\t6\t" . (17 18 10 14))
       ("3\t7\t" . (11 15))
       ("1\t5\t" . (9 13)))
       
;; output ;;
  }|

@subsubsection[#:tag "apply-agg-data-frame"]{apply-agg-data-frame}
@defproc[#:link-target? #f
 (apply-agg-data-frame [function-name Symbol] [group-hash GroupHash])
AggValueHash?]{
Returns a new DataFrame.
}

@codeblock|{
(define columns-integer
  (list 
   (cons 'col1 (new-ISeries (vector 1 2 3 4 2 ) #f))
   (cons 'col2 (new-ISeries (vector 5 6 7 8 6) #f))
   (cons 'col3 (new-ISeries (vector 9 10 11 12 17) #f))
   (cons 'col4 (new-ISeries (vector 13 14 15 16 18) #f))))

; create new data-frame-integer
(define data-frame-integer (new-data-frame columns-integer))

(frame-write-tab data-frame-integer (current-output-port))

(displayln "data-frame-groupby aggregate mean")
(apply-agg-data-frame 'mean (data-frame-groupby data-frame-integer (list 'col1 'col2)))

(displayln "data-frame-groupby aggregate count")
(apply-agg-data-frame 'count (data-frame-groupby data-frame-integer (list 'col1 'col2)))
            
;; output ;;

col1	col2	col3	col4
1	5	9	13
2	6	10	14
3	7	11	15
4	8	12	16
2	6	17	18

data-frame-groupby aggregate mean
'#hash(("4\t8\t" . 14) ("1\t5\t" . 11) ("3\t7\t" . 13) ("2\t6\t" . 14.75))
data-frame-groupby aggregate count
'#hash(("4\t8\t" . 2) ("1\t5\t" . 2) ("3\t7\t" . 2) ("2\t6\t" . 4))

;; output ;;
  }|

@section[#:style 'toc]{Loading}

Loading data from various data sources is important for any DataFrames implementation.
RacketFrames supports delimited files such as CSV (comma-separated value) as well as SQL Lite databases through the Racket SQL Lite driver.

@subsection[#:style 'toc]{Delimitted Files}
Read CSV (comma-separated or other delimitted) file into DataFrame.

@codeblock|{
; read csv
(define salary-data-frame-csv-schema (load-csv-file "../sample-csv/salary.csv" #:schema salary-schema))

(data-frame-head salary-data-frame-csv-schema)

; no schema
(define salary-data-frame-csv-no-schema (load-csv-file "../sample-csv/salary.csv" #:schema #f))

(data-frame-head salary-data-frame-csv-no-schema)

; read delimited
(define random-demographic-data-frame-delimited (load-delimited-file "../sample-csv/random_demographic.csv" "|" #:schema random-demographic-schema))

(data-frame-head random-demographic-data-frame-delimited)

; no schema
(define fruits-data-frame-delimited-no-schema (load-delimited-file "../sample-csv/random_demographic.csv" "|" #:schema #f))

(data-frame-head fruits-data-frame-delimited-no-schema) }|

@subsection[#:style 'toc]{Databases}
@codeblock|{
(require typed/db)
            
(define data-frame-from-sql-genres (data-frame-from-sql (sqlite3-connect #:database "db/chinook.db") #f "SELECT * FROM genres" empty))

(data-frame-head data-frame-from-sql-genres) }|

@tabular[#:sep @hspace[1]
(list (list @bold{GenreId} @bold{Name})
(list "1" "Rock")
(list "2" "Jazz")
(list "3" "Metal")
(list "4" "Alternative & P")
(list "5" "Rock And Roll")
(list "6" "Blues")
(list "7" "Latin")
(list "8" "Reggae")
(list "9" "Pop")
(list "10" "Soundtrack"))]

@section[#:style 'toc]{Plotting}

@codeblock|{
(displayln "plotting integer series")

(make-scatter-plot (new-ISeries (vector 1 2 3 4 5) #f))

(make-line-plot (new-ISeries (vector 1 2 3 4 5) #f)) }|

@(define-runtime-path integer-scatter-line-plots-png "integer_scatter_line_plots.png")
@image[integer-scatter-line-plots-png]

@codeblock|{
(define float-column (cons 'col1 (new-NSeries (flvector 1.5 2.5 3.5 4.5) #f)))

(displayln "plotting float columns")

(make-scatter-plot float-column)

(make-line-plot float-column)  }|

@(define-runtime-path float-scatter-line-plots-png "float_scatter_line_plots.png")
@image[float-scatter-line-plots-png]

@codeblock|{
;******************
;data-frame-integer
;******************
(define integer-columns
  (list 
   (cons 'col1 (new-ISeries (vector 1 2 3 4 4) #f))
   (cons 'col2 (new-ISeries (vector 5 6 7 8 24) #f))
   (cons 'col3 (new-ISeries (vector 9 10 11 12 24) #f))))

; create new data-frame-integer
(define data-frame-integer (new-data-frame integer-columns))

(displayln "plotting integer data-frame")

(make-scatter-plot data-frame-integer)

(displayln "plotting integer columns")

(make-scatter-plot integer-columns)
}|

@(define-runtime-path integer-columns-dataframe-plots-png "integer_columns_dataframe_plots.png")
@image[integer-columns-dataframe-plots-png]

@codeblock|{
;******************
;data-frame-float
;******************
(define float-columns
  (list 
   (cons 'col1 (new-NSeries (flvector 1.5 2.5 3.5 4.5) #f))
   (cons 'col2 (new-NSeries (flvector 5.5 6.5 7.5 8.5) #f))
   (cons 'col3 (new-NSeries (flvector 9.5 10.5 11.5 12.5) #f))))

; create new data-frame-float
(define data-frame-float (new-data-frame float-columns))

(displayln "plotting float data-frame")

(make-scatter-plot data-frame-float)

(displayln "plotting float columns")

(make-scatter-plot float-columns)

(displayln "discrete histogram")

(make-discrete-histogram (new-ISeries (vector 1 2 2 3 4 5 5 5) #f))

(make-discrete-histogram (new-GenSeries (vector 1 2 2 1.5 3 4 5 5 'a 5) #f))

(make-discrete-histogram float-column)

(make-discrete-histogram integer-columns)

(make-discrete-histogram data-frame-float)

(get-index-vector (new-ISeries (vector 1 2 3 4 4) #f))

(define series-integer-labeled
  (new-ISeries (vector 1 2 3 4)
               (list 'a 'b 'c 'd)))

(get-index-vector series-integer-labeled)

(make-discrete-histogram-stacked float-column)

(make-discrete-histogram-stacked float-columns)

;******************
;data-frame-integer
;******************
(define integer-columns-2
  (list 
   (cons 'col1 (new-ISeries (vector 1 2 3 4)
                            (build-index-from-list (list 'a 'b 'c 'd))))
   (cons 'col2 (new-ISeries (vector 5 6 7 8)
                            (build-index-from-list (list 'e 'f 'g 'h))))
   (cons 'col3 (new-ISeries (vector 9 10 11 12)
                            (build-index-from-list (list 'i 'j 'k 'l))))))

; create new data-frame-integer
(define data-frame-integer-2 (new-data-frame integer-columns-2))

(make-discrete-histogram-stacked data-frame-integer-2) }|

@section[#:style 'toc]{Aggregation}

@local-table-of-contents[]

@index-section[]
