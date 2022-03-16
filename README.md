# RacketFrames

This is RacketFrames implemented as a masters thesis at California Polytechnic State University, San Luis Obispo. DataFrames from the popular Pandas library implemented in Racket.

## Getting Started

Today procedural and functional programmers alike are working with large amounts of data, and analytical libraries have been developed for many domains to work with such data. Python/Pandas, R or even the command prompt can be used to accomplish these tasks, but this option is especially good if you are 1) using Racket already 2) may need to integrate your solution with other Racket applications in the future or 3) just plain love functional programming.

### Prerequisites

[Racket Version 6.8](http://racket-lang.org/download/) or above.

### Installing

Step by step instructions on getting up and running.

```
Download Racket Version 6.8+ for your OS from the provided prerequisite link.
```

### Documentation

The documentation is a work in progress.

[Documentation](http://htmlpreview.github.com/?https://github.com/bommysk/RacketFrames/blob/master/documentation/documentation.html)

#### Example Usage
```
#lang typed/racket

(require RacketFrames)

;******************
;data-frame-mix
;******************
(define columns-mix
  (list
   (cons 'integer-col (new-ISeries (list 1 2 3 4)
                            #:index (list 'a 'b 'c 'd)))
   (cons 'categorical-col (new-CSeries (list 'hello 'world 'fizz 'buzz)))))

; Let RacketFrames determine the Series type
(define columns-mix-2
  (list
   (cons 'integer-col (new-series (list 1 2 3 4)
                            #:index (list 'a 'b 'c 'd)))
   (cons 'categorical-col (new-series (list 'hello 'world 'fizz 'buzz)))))

; create new data-frame-mix
(define data-frame-mix (new-data-frame columns-mix))

(data-frame-write-delim data-frame-mix)

(show-data-frame-description (data-frame-description data-frame-mix))

; create new data-frame-mix-2
(define data-frame-mix-2 (new-data-frame columns-mix))

(data-frame-write-delim data-frame-mix-2)

(data-frame-write-delim data-frame-mix)

(show-data-frame-description (data-frame-description data-frame-mix-2))

;; Output ;;
Index integer-col categorical-col
0 1 0 hello
1 2 1 world
2 3 2 fizz
3 4 3 buzz
DataFrame::(Cols: 2, Rows: 4)
  - integer-col: IntegerSeries
  - categorical-col: CategoricalSeries
Index integer-col categorical-col
0 1 0 hello
1 2 1 world
2 3 2 fizz
3 4 3 buzz
Index integer-col categorical-col
0 1 0 hello
1 2 1 world
2 3 2 fizz
3 4 3 buzz
DataFrame::(Cols: 2, Rows: 4)
  - integer-col: IntegerSeries
  - categorical-col: CategoricalSeries
;; Output ;;
```

```
; no schema
; from root of repository
(define salary-data-frame-csv-no-schema (load-csv-file "racketframes/sample-csv/salary.csv"))

(data-frame-head salary-data-frame-csv-no-schema)

;; Output ;;
Index     first           last             age           dollar           phone         join_date    
0 '(Evan) 0 '(Lloyd)  0 '(19) 0 '($3839.78) 0 '(|(771) 255-1133|) 0 '(2018-05-19) 
1 '(Etta) 1 '(Griffith) 1 '(50) 1 '($8158.60) 1 '(|(523) 731-6388|) 1 '(2018-05-19) 
2 '(William)  2 '(Conner) 2 '(50) 2 '($9966.70) 2 '(|(759) 504-6619|) 2 '(2018-05-19) 
3 '(Rhoda)  3 '(Guerrero) 3 '(20) 3 '($6480.10) 3 '(|(467) 431-4273|) 3 '(2018-05-19) 
4 '(Kyle) 4 '(Klein)  4 '(59) 4 '($6106.13) 4 '(|(760) 829-2093|) 4 '(2018-05-19) 
5 '(Benjamin) 5 '(Patton) 5 '(59) 5 '($3925.51) 5 '(|(488) 673-5745|) 5 '(2018-05-19) 
6 '(Georgie)  6 '(Hansen) 6 '(51) 6 '($8809.92) 6 '(|(579) 706-4402|) 6 '(2018-05-19) 
7 '(Gregory)  7 '(Bowen)  7 '(36) 7 '($5176.21) 7 '(|(533) 506-3845|) 7 '(2018-05-19) 
8 '(Cornelia) 8 '(Peterson) 8 '(46) 8 '($3626.31) 8 '(|(861) 316-5672|) 8 '(2018-05-19) 
9 '(Samuel) 9 '(Cole) 9 '(37) 9 '($7677.20) 9 '(|(760) 406-6331|) 9 '(2018-05-19) 
;; Output ;;
```

```
(displayln "DataFrame List of Column Names")
(data-frame-names salary-data-frame-csv-no-schema)

;; Output ;;
DataFrame List of Column Names
'(first last age dollar phone)
;; Output ;;

(displayln "DataFrame Dimensions")
(data-frame-dim salary-data-frame-csv-no-schema)

;; Output ;;
DataFrame Dimensions
(Dim 200 5)
;; Output ;;

(displayln "DataFrame Description")
(show-data-frame-description (data-frame-description salary-data-frame-csv-no-schema))

;; Output ;;
DataFrame Description
DataFrame::(Cols: 6, Rows: 200)
  - first: GenericSeries
  - last: GenericSeries
  - age: GenericSeries
  - dollar: GenericSeries
  - phone: GenericSeries
  - join_date: GenericSeries
;; Output ;;
```

## Running the tests

Unit tests can be run from dataframe/tests which contains individual test files written in Racket using typed/rackunit.

### Benchmarks

One of the purposes of this work is to compare performance with other DataFrame implementations. Currently benchmarks are being implemented for comparison against Pandas.

Benchmarks are located in the benchmark directory and separated into different directories by the type of benchmark. They are shells scripts that print the performance of the benchmark against that of Pandas.

Example:
```
cd racketframes/benchmark/Pandas/join_merge
sh join_merge.sh
```

## Authors

* **Shubham Kahal**

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details.
