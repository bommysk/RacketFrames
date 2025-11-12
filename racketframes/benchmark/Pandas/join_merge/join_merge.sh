#!/bin/bash

echo "**********"
echo "Concat Benchmark Comparisons"
echo "**********"
raco make concat-benchmarks.rkt
racket concat-benchmarks.rkt
echo "**********\n\n"

echo "**********"
echo "Append Benchmark Comparisons"
echo "**********"
raco make append-benchmarks.rkt
racket append-benchmarks.rkt
echo "**********\n\n"

echo "**********"
echo "Join Benchmark Comparisons"
echo "**********"
raco make join-benchmarks.rkt
racket join-benchmarks.rkt
echo "**********\n\n"
