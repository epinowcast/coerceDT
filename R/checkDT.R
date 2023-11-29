#' @title Coerce `data.table`s
#'
#' @description Provides consistent coercion of `data` to [data.table]
#' with error handling.
#'
#' @param data Any of the types supported by [data.table::as.data.table()] OR
#' a single character string, in which case [data.table::fread()] is used to
#' read in `data`.
#'
#' @param required Optional; if `NULL` (the default), there are no required
#' columns. If a `character` vector, `coerceDT` will produce an error indicating
#' which columns are not present. If a named `list`, the names will be required
#' columns and the `list` entries will be used to check the corresponding
#' columns. If those entries are themselves characters, those will be assumed to
#' be the class to check to via an `is.Class` method; otherwise, they should be
#' single argument functions that will be used to transform the column.
#'
#' @param forbidden Optional; if `NULL`, ignored. If a character vector,
#' `coerceDT` will error if any of those columns are present.
#'
#' @inheritDotParams data.table::fread
#'
#' @return A `data.table`; the returned object will be a copy (default), unless
#' `copy = FALSE`, in which case modifications *may* be made in-place, though
#' are not *guaranteed* to have been so
#'
#' @details This function provides a general-purpose tool for common, basic
#' checking and conversion tasks with `data.table`s. It's intended use is as
#' a simplifying, standardizing interface for raw input checking, not to perform
#' complex requirement checks or manipulations. It is not, e.g., able to answer
#' if one-and-only-one of some set of columns are present, or to coerce column
#' values to a new values based on anything other than their initial value.
#'
#'
#'
#' This method is implemented as an S3 generic method, and dispatches according
#' to `class(data)`. The class-specific methods may be used directly; *however*,
#' that will skip some input checking, namely:
#'  - that `select` / `drop` are mutually exclusive
#'  - that `copy` must be a length 1 logical
#'  - that `required` / `forbidden` have the correct format
#'
#' @export
checkDT <- function(
  data,
  required = NULL, forbidden = NULL,
  na.error = TRUE, warn = TRUE,
  ...
) {

}
