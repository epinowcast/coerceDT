#' @title Check `data.table`s
#'
#' @description Provides a checking interface for `data.table`s
#'
#' @param data a `data.table`, e.g. as a product of [coerceDT()]
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
#' @return `data` itself, assuming passing required and forbidden
#'
#' @details This function provides a general-purpose tool for common, basic
#' checking and conversion tasks with `data.table`s. It's intended use is as
#' a simplifying, standardizing interface for raw input checking, not to perform
#' complex requirement checks or manipulations. It is not, e.g., able to answer
#' if one-and-only-one of some set of columns are present, or to coerce column
#' values to a new values based on anything other than their initial value.
#'
#' @export
checkDT <- function(
  data,
  required = NULL, forbidden = NULL
) {
  if (!is.null(required)) {
    stop()
  }
  if (!is.null(forbidden)) {
    stop()
  }
  data
}
