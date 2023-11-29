#' @title Coerce `data.table`s
#'
#' @description Provides consistent coercion of `data` to [data.table]
#' with error handling.
#'
#' @param data Any of the types supported by [data.table::as.data.table()] OR
#' a single character string. If a string, checked against the pattern `\\.rds$`;
#' if match, uses [readRDS()], otherwise uses [data.table::fread()].
#'
#' @param copy Logical; if `TRUE` (default), a new `data.table` is returned;
#' if `FALSE`, `data` *may* be modified in place, but is not *guaranteed* to be
#' so. For example, selecting a subset of columns creates a new `data.table`
#' reference.
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
#' This method is implemented as an S3 generic method, and dispatches according
#' to `class(data)`. The class-specific methods may be used directly; *however*,
#' that will skip some input checking, namely:
#'  - that `select` / `drop` are mutually exclusive
#'  - that `copy` must be a length 1 logical
#'  - that `required` / `forbidden` have the correct format
#'
#' @export
coerceDT <- function(
  data, ..., copy = TRUE
) {
  UseMethod("coerceDT")
}

#' @rdname coerceDT
#' @importFrom data.table as.data.table setDT
#' @export
coerceDT.default <- function(
  data, ..., copy
) {
  res_ <- if (copy) as.data.table(data) else setDT(data)
  return(internal_select_drop_convert(res_, ...))
}

#' @rdname coerceDT
#' @importFrom data.table fread
#' @importFrom cli cli_abort
#' @export
coerceDT.character <- function(
  data, ..., copy
) {
  if (length(data) != 1) {
    cli::cli_abort(c(
      "x" = "Argument {.var data} must have length 1.",
      "i" = "Supplied `length(data) == {length(data)}`."
    ))
  }
  isRDS <- grepl(pattern = "\\.rds$", x = data, ignore.case = TRUE)
  if (isRDS) {
    # readRDS data may require further coercion, but won't need to be copied
    return(coerceDT.default(readRDS(data), ..., copy = FALSE))
  } else {
    # fread sets the interface standard, so is just complete
    return(data.table::fread(input = data, ...))
  }
}

#' TODO
#' To normalize behavior with `fread`, need to support these fread arguments
#' to modify an existing `data.table`.
internal_select_drop_convert <- function(
  dt, nrows, stringsAsFactors, select, drop, colClasses, col.names, check.names,
  strip.white, key, logical01, tz, ...
) {

  return(dt)
}
