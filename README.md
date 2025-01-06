
# makeDT

<!-- badges: start -->
<!-- badges: end -->

## Quickstart

```r
remotes::install_github("epinowcast/makeDT")
# TODO: install.package("makeDT")
library(makeDT)
mtcars_dt <- makeDT(mtcars, select = list(hp = "integer", "wt"))
mtcars_dt
```

## Motivation

The point of `makeDT` is to streamline ingest-and-check tasks for user-provided data, yielding a `data.table` for subsequent operations OR useful error messages. The exported functions are meant as internal functions for data science pipeline packages, potentially on large datasets and/or with many repetitions, so performance is a key concern.

For developers, `{makeDT}` should *simplify* the combination of typical ingest-and-check operations, so must be preferable to the alternative of writing their own boilerplate reading / checking steps. To that end, we leverage the existing "grammar" of `data.table` while providing a focused mini-language for the specific task.

That mini-language address two basic questions: what *must* be present in some data? and, distinctly, what *must not* be present in that data?

Whether there are constraints, `{makeDT}` also provides a uniform method to translate input in the `data.table` format: the same interface can flexibly handle an existing object (of any of the types supported by `data.table::setDT()` and `data.table::as.data.table()`), anything that `data.table::fread` would handle, a path to an `rds` file. Finally, the `{makeDT}` methods by default ensure no side-effects on the input object, but can allow side-effects to maximize performance.

## Conceptual Vocabulary

There are three operations, `castDT`, `testDT`, and `makeDT` which combine the four verbs in the `{makeDT}` vocabulary:

 - `keep` & `drop`: what columns to *include* (keep) or *exclude* (drop).
 - `expect` & `forbid`: what column content *must* (expect) or *must not* (forbid) be present.

Essentially, `castDT` turns some object into a `data.table` without performing any checks. `testDT` tests if an existing `data.table` complies with a specification, but won't do anything to convert the input to a `data.table`. Lastly, `makeDT` does `castDT`, then `checkDT`.

The `select` and `drop` verbs are arguments to `castDT()`. The `expect` and `forbid` verbs are used in `checkDT()`.
All of the verbs may be combined in `makeDT()`.

At this time, the complementary verbs are mutually exclusive - e.g. `castDT(data, select = ..., drop = ...)` will emit an error. However, users should not rely on this error behavior for program logic: we may eventually relax these hard errors to enable convenient behavior like "transform this column this way, drop this column, and keep the rest as-is".

The cross verbs can work together:

 1. `select` and `forbid` will automatically work if they name mutually exclusive columns .


## Detailed Vocabulary

### `select` & `drop`.

The basic `select` argument is a character vector: `c("colA", "colB")` selects two columns. The complementary argument, `drop`, is just a character vector. Specifying overlapping columns for `select` and `drop` is an error.

When not supplied, `select` is assumed to be all the columns, minus any specified in `drop`.

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

If you want all your columns as base classes, e.g. `colA` as integers, then you can use `castDT(data, expect = c(colA = "integer", ...), ...)`. In that example, `castDT` will effectively promote this to `list(colA = is.integer, ...)`. Any `is.XYZ` available in the environment will be accessible by `list(colA = "XYZ")`.

Lastly, if you have a more testing operation, e.g. converting a character column that included numbers recorded as fractions, the you can use the full semantics by providing a custom test function

### `forbid`

## `copy`ing

The use of the `copy` argument is at the core of maintaining performance with `castDT`. For some operations, `castDT` will internally manage when copies are *not* made - e.g. in general, `data.table` selections provide new objects and preclude modification of the input object, so in these cases it is unnecessary to make an additional copy.

Otherwise, by default, `castDT` will ensure its input is not modified by creating a new object. This is generally the appropriate guarantee for some user-facing function. However, that behavior might be undesirable for performance reasons, _e.g._ if that guarantee is otherwise enforced like when `castDT` is used in a series of internal function calls.

## Inner Workings

A typically developer should be using `castDT`. However, that method is actually a gateway to several other functions, which handle particular types of `data` and translate the verbs accordingly. For data coming from the file system, those underlying readers often support elements of the `castDT` vocabulary, but with different names or format, hence translation is required.

The general idea is to *not* repeat steps, both in terms of what the code does, but also in how the code is written. This means that some steps do part of the necessary work, then pass `data` off to other methods, after modifying arguments.

These functions are exported, and thus available to use directly. However, the entry point `castDT` function checks the verbs but the class-specific versions *do not*.

## Fail Fast versus Fail Thorough

TODO: should our philosophy be to fail on the first error, or to collect errors as far as possible into `castDT`ing, and then report out?

The first option is easiest to implement (by a long stretch), but the second is probably the most useful to people?

We could split the difference by making the error contract loose, MVP the first option, and then gradually work towards the second?

## Installation

You can install the development version of `makeDT` using the `remotes` package:

```r
remotes::install_github("epinowcast/makeDT")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(makeDT)
## basic example code
```

## More Motivation

The `makeDT` function grew out of `{epinowcast}` package needs. We intend the
functions in `{epinowcast}` to be assembled into pipelines by other users, while
also providing our own pipelining. This generally means proper handling of
user-space data, namely: no surprise side effects, but also no assumptions about
the validity of that data. At the same time, we want to avoid unnecessarily
copying and checking data passed from other parts of the library (or for
advanced users that are fine with side-effects to data).
