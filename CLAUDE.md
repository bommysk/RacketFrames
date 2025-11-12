# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

RacketFrames is a DataFrame library for Racket (similar to Python's Pandas), implemented as a masters thesis project. It provides data manipulation, analysis, and visualization capabilities for large datasets in a functional programming paradigm.

## Prerequisites

- Racket Version 6.8 or above
- Dependencies listed in `info.rkt`: base, math-lib, plot-gui-lib, plot-lib, rackunit-typed, typed-racket-lib, typed-racket-more

## Running Tests

Unit tests are located in `racketframes/tests/` and are written using typed/rackunit. To run tests:

```bash
# Run individual test files from the tests directory
racket racketframes/tests/<test-file>.rkt

# Examples
racket racketframes/tests/data-frame.rkt
racket racketframes/tests/series-iter.rkt
```

Functional tests are in `racketframes/functional_tests/`.

## Benchmarks

Benchmarks compare RacketFrames performance against Pandas. Located in `racketframes/benchmark/Pandas/`, they are shell scripts that execute both Racket and Python implementations.

To run a benchmark:
```bash
cd racketframes/benchmark/Pandas/join_merge
sh join_merge.sh
```

Available benchmarks:
- `join_merge/join_merge.sh` - Join/merge operations
- `indexing/indexing_benchmarks.sh` - Indexing operations

## Architecture

### Core Type System

The library uses Typed Racket and is built around a type hierarchy defined in `racketframes/data-frame/types.rkt`:

**Series Types** (columns of uniform data):
- `GenericSeries` - Generic/mixed data types
- `IntegerSeries` - Integer data with optimized operations
- `NumericSeries` - Floating-point numeric data
- `CategoricalSeries` - Categorical/symbolic data with efficient storage
- `BooleanSeries` - Boolean data
- `DatetimeSeries` - Date and time data

**DataFrame**: Collection of Series with labeled columns, similar to a SQL table or spreadsheet.

### Main Entry Point

`racketframes/main.rkt` is the primary module that:
- Re-exports all public APIs from submodules
- Serves as the entry point for users: `(require RacketFrames)`

### Key Subsystems

**Data Loading** (`racketframes/load/`):
- `load.rkt` - Main loading interface
- `csv-delimited.rkt` / `delimited.rkt` - CSV and delimited file parsers
- `schema.rkt` / `schema-syntax.rkt` - Schema definition and inference
- `data-frame-builder.rkt` - Constructs DataFrames from parsed data

**DataFrame Operations** (`racketframes/data-frame/`):
- `data-frame.rkt` - Core DataFrame structure and accessors (loc, iloc, project, extend, etc.)
- `data-frame-ops.rkt` - Operations on DataFrames (filtering, sorting, aggregation)
- `data-frame-join.rkt` - Join/merge operations
- `data-frame-concat.rkt` - Concatenation operations
- `data-frame-print.rkt` - Display and printing utilities
- `groupby-util.rkt` - GroupBy functionality for split-apply-combine operations

**Series Operations**:
- `series.rkt` - Base Series interface
- `indexed-series.rkt` - Series with index support
- `*-series.rkt` files - Type-specific Series implementations
- `*-series-builder.rkt` files - Builder pattern for constructing Series
- `*-series-ops.rkt` files - Type-specific operations (arithmetic, comparison, etc.)

**Utilities**:
- `racketframes/util/` - List operations, datetime utilities, formatting
- `racketframes/format/` - Data encoding/formatting
- `racketframes/plot/` - Plotting and visualization
- `racketframes/validation/` - Data validation utilities

### Builder Pattern

The library uses the builder pattern extensively for constructing Series and DataFrames:
1. Create a builder (e.g., `new-ISeriesBuilder`, `new-CSeriesBuilder`)
2. Append data incrementally
3. Complete the builder to get an immutable Series

### Schema System

Schemas specify column types and headers when loading data:
- Use `(schema ...)` macro from `schema-syntax.rkt` to define schemas
- `get-schema` can infer schemas from files
- Pass schemas to `load-csv-file` with `#:schema` parameter

### Indexing

DataFrames support both label-based (`data-frame-loc`) and position-based (`data-frame-iloc`) indexing:
- `data-frame-loc` - Label-based selection (like Pandas .loc)
- `data-frame-iloc` - Integer position-based selection (like Pandas .iloc)
- Indexes can be set via `data-frame-set-index`

## Common Usage Patterns

**Creating DataFrames**:
```racket
; From Series
(define df (new-data-frame
  (list (cons 'col1 (new-ISeries (list 1 2 3)))
        (cons 'col2 (new-CSeries (list 'a 'b 'c))))))

; From CSV
(define df (load-csv-file "path/to/file.csv"))
(define df-with-schema (load-csv-file "path.csv" #:schema my-schema))
```

**Accessing Data**:
```racket
; Get column names
(data-frame-series-names df)

; Get dimensions
(data-frame-dim df)

; Access by label
(data-frame-loc df 'row-label '(col1 col2))

; Access by position
(data-frame-iloc df 0 1)  ; row 0, column 1
```

**Operations**:
```racket
; Project columns
(data-frame-project df '(col1 col3))

; Filter rows (via Series boolean operations)
; Join DataFrames
(data-frame-join df1 df2 ...)

; Group by
(data-frame-groupby df '(group-col) agg-functions)
```

## Sample Data

Sample CSV files are available in `racketframes/sample-csv/` for testing and examples.
