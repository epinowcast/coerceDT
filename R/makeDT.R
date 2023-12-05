#' @title Make `data.table`s
#'
#' @description Combines [coerceDT()] and [checkDT()] to provide consistent
#' coercion of `data` to [data.table] with error handling.
#'
#' @inheritParams coerceDT
#' @inheritParams checkDT
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
#' @export
makeDT <- function(
  data,
  select, drop,
  expect, forbid,
  copy = TRUE
) {
  doargs_coerce <- list(data = data, copy = copy)
  if (!missing(select)) doargs_coerce$select <- select
  if (!missing(drop)) doargs_coerce$drop <- drop
  doargs_check <- list(data = do.call(coerceDT, doargs_coerce))
  if (!missing(expect)) doargs_check$expect <- expect
  if (!missing(forbid)) doargs_check$forbid <- forbid
  do.call(checkDT, doargs_check)
}
