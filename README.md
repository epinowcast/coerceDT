
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

The point of `coerceDT` is to standardize basic ingest-and-check tasks for user-provided data, yielding a `data.table` for subsequent operations OR useful error messages. The `coerceDT` function is intended for use in data science pipelines, potentially on large objects and/or with many repetitions, so must have minimal overhead while ensuring no side effects.

For developers, `coerceDT` should *simplify* the combination of typical ingest-and-check operations, so must be preferable to the alternative of writing their own combination of boilerplate reading / checking steps. That means we leverage the existing vocabulary of `data.table` while providing a focused mini-language for the core ingest-and-check steps.

That mini-language address two basic questions: what *must* be present in some data? and, distinctly, what *must not* be present in that data? Notably: there may also be *no constraints* on some data.

Whether there are constraints, `coerceDT` provides a uniform method to getting some input in the `data.table` format. The same interface can flexibly handle a file path or existing object. Likewise, it can be used to enforce no side-effects on the input object, or allow those side-effects to maximize performance.

## Conceptual Vocabulary

The argument signature of `coerceDT` is:

```r
TODO block of stuff - make this an Rmd, so it can be generated from the function itself?
```

This signature covers the core _constraints_ with `drop`, `forbidden`, `required`, and `select` arguments.

### *Must* versus *Must Not*

The *must* aspect is addressed by the `required` and `select` arguments. The `required` argument is enforced first, which will ensure the presence (and optionally class) of the data. The `select` argument will then select the relevant columns, possibly transformed.

The *must not* aspect is addressed by the `drop` and `forbidden` arguments. The `drop` argument is enforced first, removing columns (and optionally, rows meeting some criteria). The `forbidden` argument will then ensure the absence of columns (and optionally, rows meeting some criteria).

### `copy`ing

The use of the `copy` argument is at the core of maintaining performance with `coerceDT`. For some operations, `coerceDT` will internally manage when copies are *not* made - e.g. in general, `data.table` selections provide new objects and preclude modification of the input object, so in these cases it is unnecessary to make an additional copy.

Otherwise, by default, `coerceDT` will ensure its input is not modified by creating a new object. This is generally the appropriate guarantee for some user-facing function. However, that behavior might be undesirable for performance reasons, _e.g._ if that guarantee is otherwise enforced like when `coerceDT` is used in a series of internal function calls.

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

