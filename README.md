
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

For developers, `coerceDT` should *simplify* the combination of typical ingest-and-check operations, so must be preferable to the alternative of writing their own combination of boilerplate reading / checking steps. That means we leverage the existing vocabulary of `data.table` while providing a focused mini-language for the core ingest-and-check steps. When inputs are paths, `coerceDT` should normalize (to the extent possible) the arguments between the supported mimetype readers.

That mini-language address two basic questions: what *must* be present in some data? and, distinctly, what *must not* be present in that data? Notably: there may also be *no constraints* on some data.

Whether there are constraints, `coerceDT` provides a uniform method to getting some input in the `data.table` format. The same interface can flexibly handle a file path or existing object. Likewise, it can be used to ensure no side-effects on the input object, or allow those side-effects to maximize performance.

## Conceptual Vocabulary

The are four verbs in the `coerceDT` vocabulary:

 1. `required`: what column content *must* be present in the input, by default in terms of the existence of column and optionally also what `class` that column should be coerce-able-to.
 2. `forbidden`: what column content *must not* be present in the input, by default in terms of entire columns but optionally in terms of a test that all entries in the column must satisfy.
 3. `select`: what columns to include or exclude. If *included* columns are not present, leads to an error, but *excluded* columns may or may not be present in the input.
 4. `rename`: convert column names to new names

These verbs are applied in that order, so the early ones can be used to enforce guarantees for later ones. The function itself, however, does not guarantee any "fail fast" logic; e.g. there is no guarantee that `select` included columns will be checked at the `required` and thus failure will occur prior to checking the `forbidden` logic. Similarly, `required` and `forbidden` are enforcing constraints on the input, *not* making guarantees on the output. Rather, guarantees are a result of how a developer *combines* the verbs.

Aside: `select` and `rename` *may* be eventually combined. The goal is to support both pattern matching and non-standard evaluation. The selection syntax would be roughly to exclude `-something(s)` or `-patterns(...)`, to include `something` or `pattern(...)` OR to include-AS `something = else` or `pattern(...) = \(n) somenametransformation`, and lastly "." to include everything else.

The argument signature of `coerceDT` is:

```r
TODO block of stuff - make this an Rmd, so it can be generated from the function itself?
```

## Detailed Vocabulary

### `required`

The `required` argument ultimately takes the form

```r
list(colA = as.required(x), colB = ..., ...)
```

However, users don't have to fully provide this specification. By default:

```r
as.required = \(x) x         # i.e., just use the value
```

If you want to ensure the presence of `colA`, `colB`, etc but have no other constraints, then `coerceDT(data, required = c("colA", "colB", ...), ...)` will suffice: `coerceDT` will effectively promote plain strings to the names of list. A `list` of string entries will also work.

If you want all your columns as base classes, e.g. `colA` as integers, then you can use `coerceDT(data, required = c(colA = "integer", ...), ...)`. In that example, `coerceDT` will effectively promote this to `list(colA = as.integer, ...)`. Any `as.XYZ` available in the environment will be accessible by `list(colA = "XYZ")`.

Lastly, if you have a more complicated casting operation, e.g. converting a character column that included numbers recorded as fractions, the you can use the fully semantics by providing a custom casting function:

```r
numeric_from_fraction = \(x) {
  res <- numeric(length(x))
  fracs <- grepl("/", x)
  res[fracs] <- sapply(x[fracs], \(f) eval(parse(text = f)), USE.NAMES = FALSE)
  res[!fracs] <- as.numeric(x[!fracs])
  return(res)
}
coerceDT(data, required = list(colA = numeric_from_fraction, ...), ...)
```

The `required` argument also considers on the `na.error` argument. If any required columns have any `NA` values after casting (either from the initial data or via failures to the casting process), by default, `coerceDT`ing has failed and throws an error. If `na.error = FALSE`, `NA`s are allowed by this stage (though can still be enforced by `forbidden`, see next section).

### `forbidden`

The `forbidden` argument ultimately takes the form

```r
list(colA = is.forbidden(x), colB = ..., ...)
```

However, users don't have to fully provide this specification. By default:

```r
is.forbidden = \(x) TRUE         # i.e., just return true
```

If you want to prohibit the presence of `colA`, `colB`, etc but have no other constraints, then `coerceDT(data, forbidden = c("colA", "colB", ...), ...)` will suffice: `coerceDT` will effectively promote plain strings to the names of list. A `list` of string entries will also work. The default function will then indicate that any value in the column will return `TRUE`, meaning that value is forbidden, thus the only way a data structure will pass is if the column is *absent* entirely.

If you want more precise checks, e.g. that `colA` must be non-negative, then you could use `coerceDT(data, forbidden = list(colA = \(x) x < 0))`. Note carefully that test should return `TRUE` when the value is *forbidden* (versus returning `TRUE` if allowed).

### `select`

### `rename`

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

