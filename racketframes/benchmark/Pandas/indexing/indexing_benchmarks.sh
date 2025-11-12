#!/bin/bash

echo "**********"
echo "Integer Series Benchmark Comparisons"
echo "**********"
raco make integer-series-benchmarks.rkt
racket integer-series-benchmarks.rkt
echo "**********\n\n"

echo "**********"
echo "Numeric Series Benchmark Comparisons"
echo "**********"
raco make numeric-series-benchmarks.rkt
racket numeric-series-benchmarks.rkt
echo "**********\n\n"

echo "**********"
echo "Categorical Series Benchmark Comparisons"
echo "**********"
raco make categorical-series-benchmarks.rkt
racket categorical-series-benchmarks.rkt
echo "**********\n\n"
