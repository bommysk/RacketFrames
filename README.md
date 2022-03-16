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

; create new data-frame-mix
(define data-frame-mix (new-data-frame columns-mix))

(frame-write-tab data-frame-mix)
```

```
; no schema
(define salary-data-frame-csv-no-schema (load-csv-file "sample-csv/salary.csv" #:schema #f #:delim ","))

(data-frame-head salary-data-frame-csv-no-schema)

;; Output ;;
     first           last             age           dollar           phone      
  Evan            Lloyd             19           $3839.78        (771) 255-1133  
  Etta            Griffith          50           $8158.60        (523) 731-6388  
  William         Conner            50           $9966.70        (759) 504-6619  
  Rhoda           Guerrero          20           $6480.10        (467) 431-4273  
  Kyle            Klein             59           $6106.13        (760) 829-2093  
  Benjamin        Patton            59           $3925.51        (488) 673-5745  
  Georgie         Hansen            51           $8809.92        (579) 706-4402  
  Gregory         Bowen             36           $5176.21        (533) 506-3845  
  Cornelia        Peterson          46           $3626.31        (861) 316-5672  
  Samuel          Cole              37           $7677.20        (760) 406-6331 
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
DataFrame::(Cols: 5, Rows: 200)
  - first: CategoricalSeries
  - last: CategoricalSeries
  - age: IntegerSeries
  - dollar: CategoricalSeries
  - phone: CategoricalSeries
;; Output ;;
```

## Running the tests

Unit tests can be run from dataframe/tests which contains individual test files written in Racket using typed/rackunit.

### Benchmarks

One of the purposes of this work is to compare performance with other DataFrame implementations. Currently benchmarks are being implemented for comparison against Pandas.

Benchmarks are located in the benchmark directory and separated into different directories by the type of benchmark. They are shells scripts that print the performance of the benchmark against that of Pandas.

Example:
```
sh benchmark/join_merge/join_merge.sh
```

## Authors

* **Shubham Kahal**

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details.
