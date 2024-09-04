
# coerceDT

<!-- badges: start -->
<!-- badges: end -->

## Quickstart

```r
remotes::install_github("epinowcast/coerceDT")
# TODO: install.package("coerceDT")
require(coerceDT)
dtcars <- coerceDT(mtcars)
```

## Motivation

The point of `coerceDT` is to standardize basic ingest-and-check tasks for user-provided data, yielding a `data.table` for subsequent operations OR useful error messages. We intend the exported functions for use in data science pipelines, potentially on large dataset and/or with many repetitions, so want to have minimal overhead while ensuring no side effects.

For developers, `{coerceDT}` should *simplify* the combination of typical ingest-and-check operations, so must be preferable to the alternative of writing their own combination of boilerplate reading / checking steps. That means we leverage the existing vocabulary of `data.table` while providing a focused mini-language for the core ingest-and-check steps.

That mini-language address two basic questions: what *must* be present in some data? and, distinctly, what *must not* be present in that data? Notably: there may also be *no constraints* on some data.

Whether there are constraints, `coerceDT` provides a uniform method to getting some input in the `data.table` format. The same interface can flexibly handle a file path or existing object. Likewise, it can be used to ensure no side-effects on the input object, or allow those side-effects to maximize performance.

## Conceptual Vocabulary

The are four (plus one) verbs in the `coerceDT` vocabulary:

 1. `select`: what columns to include, and potentially coerce to a particular type. If selected columns are not present, leads to a warning. `select` also has the "plus one verb": `default`, which will provide select values if they are absent.
 2. `drop`: which columns to exclude
 3. `expect`: what column content *must* be present in the input, by default in terms of the existence of column and optionally also testing the column values.
 4. `forbid`: what columns *must not* be present in the input.

The `select` and `drop` verbs are mutually exclusive, and used in `coerceDT()`. The `expect` and `forbid` verbs may be combined, and are used in `checkDT()`.
All of the verbs may be combined in `makeDT()`.

## Detailed Vocabulary

### `select`

### `drop`

### `expect`

The `expect` argument ultimately takes the form

```r
list(colA = is.expected(x), colB = ..., ...)
```

However, users don't have to fully provide this specification. By default:

```r
is.expected = \(x) TRUE        # i.e., any value is fine
```

If you want to ensure the presence of `colA`, `colB`, etc but have no other constraints, then `checkDT(data, expect = c("colA", "colB", ...))` will suffice: `checkDT` will effectively promote plain strings to the names of list.

If you want all your columns as base classes, e.g. `colA` as integers, then you can use `coerceDT(data, expect = c(colA = "integer", ...), ...)`. In that example, `coerceDT` will effectively promote this to `list(colA = is.integer, ...)`. Any `is.XYZ` available in the environment will be accessible by `list(colA = "XYZ")`.

Lastly, if you have a more testing operation, e.g. converting a character column that included numbers recorded as fractions, the you can use the fully semantics by providing a custom test function

### `forbid`

## `copy`ing

The use of the `copy` argument is at the core of maintaining performance with `coerceDT`. For some operations, `coerceDT` will internally manage when copies are *not* made - e.g. in general, `data.table` selections provide new objects and preclude modification of the input object, so in these cases it is unnecessary to make an additional copy.

Otherwise, by default, `coerceDT` will ensure its input is not modified by creating a new object. This is generally the appropriate guarantee for some user-facing function. However, that behavior might be undesirable for performance reasons, _e.g._ if that guarantee is otherwise enforced like when `coerceDT` is used in a series of internal function calls.

## Inner Workings

A typically developer should be using `coerceDT`. However, that method is actually a gateway to several other functions, which handle particular types of `data` and translate the verbs accordingly. For data coming from the file system, those underlying readers often support elements of the `coerceDT` vocabulary, but with different names or format, hence translation is required.

The general idea is to *not* repeat steps, both in terms of what the code does, but also in how the code is written. This means that some steps do part of the necessary work, then pass `data` off to other methods, after modifying arguments.

These functions are exported, and thus available to use directly. However, the entry point `coerceDT` function checks the verbs but the class-specific versions *do not*.

## Fail Fast versus Fail Thorough

TODO: should our philosophy be to fail on the first error, or to collect errors as far as possible into `coerceDT`ing, and then report out?

The first option is easiest to implement (by a long stretch), but the second is probably the most useful to people?

We could split the difference by making the error contract loose, MVP the first option, and then gradually work towards the second?

## Installation

You can install the development version of `coerceDT` using the `remotes` package:

```r
remotes::install_github("epinowcast/coerceDT")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(coerceDT)
## basic example code
```

