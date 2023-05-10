#' @title Coerce `data.table`s
#'
#' @description Provides consistent coercion of `data` to [data.table]
#' with error handling.
#'
#' @param data Any of the types supported by [data.table::as.data.table()] OR
#' a single character string, in which case [data.table::fread()] is used to
#' read in `data`.
#'
#' @param select Optional; if `NULL` (the default), all columns of `data` are
#' returned. If a `character` vector, the corresponding columns are returned. If
#' a `j`-like expression (see [data.table::data.table()]), the result of that
#' expression will be returned. _N.B._ for the `j`-like version: `required`
#' will be considered *before* selection occurs, and thus any coercion to
#' relevant classes will also happen pre-selection.
#'
#' @param drop Optional; if `NULL`, ignored. If a character or integer vector,
#' the corresponding columns *if present* will be dropped. If *not* present,
#' `coerceDT` will warn about a request to drop columns that don't exist.
#'
#' @param required Optional; if `NULL` (the default), there are no required
#' columns. If a `character` vector, `coerceDT` will produce an error indicating
#' which columns are not present. If a named `list`, the names will be required
#' columns and the `list` entries will be used to coerce the corresponding
#' columns. If those entries are themselves characters, those will be assumed to
#' be the class to coerce to via an `as.Class` method; otherwise, they should be
#' single argument functions that will be used to transform the column.
#'
#' @param forbidden Optional; if `NULL`, ignored. If a character vector,
#' `coerceDT` will error if any of those columns are present.
#'
#' @param copy Logical; if `TRUE` (default), a new `data.table` is returned;
#' if `FALSE`, `data` *may* be modified in place, but is not *guaranteed* to be
#' so. For example, selecting a subset of columns creates a new `data.table`
#' reference.
#'
#' @param NAerror Logical; if column coercion results in any `NA`s, is that an
#' error (default: yes)?
#'
#' @param warn Logical; issue a warning e.g. when `drop`ping columns that are
#' not present (default: yes)?
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
coerceDT <- function(
  data,
  select = NULL, drop = NULL,
  required = NULL, forbidden = NULL,
  copy = TRUE, NAerror = TRUE, warn = TRUE,
  ...
) {
  # select / drop: is.null(select), is.null(drop)
  # both = fine; one or the other = fine; neither = problem
  # copy: must be a length(1) logical
  stopifnot(
    "coerceDT: Use either `select=` or `drop=` but not both." =
      is.null(select) | is.null(drop)
  )
  UseMethod("coerceDT")

}

#' @rdname coerceDT
#' @importFrom data.table as.data.table setDT
#' @export
coerceDT.default <- function(
  data, ..., copy
) {
  return(coerceDT.data.table(if (copy) as.data.table(data) else setDT(data)), ..., copy = FALSE)
}

#' @rdname coerceDT
#' @importFrom data.table fread
#' @export
coerceDT.character <- function(
  data, select = NULL, drop = NULL,
  required = NULL, forbidden = NULL,
  copy = FALSE, NAerror = TRUE, warn = TRUE,
  ...
) {
  data_ <- data[1]
  if (warn & (length(data) != 1)) {
    warning("provided `data` as multiple strings; using `data[1]` only.")
  }
  isRDS <- tolower(tools::file_ext(data_)) == "rds"
  return(coerceDT.data.table(
    data = if (isRDS) {
      readRDS(data_)
    } else {
      fread(input = data_, select = select, drop = drop, ...)
    },
    select = select, drop = drop,
    required = required, forbidden = forbidden,
    copy = FALSE, NAerror = NAerror
  ))
}


#' @rdname coerceDT
#' @export
coerceDT.data.table <- function(
  data,
  select = NULL, drop = NULL,
  required = NULL, forbidden = NULL,
  copy = TRUE, NAerror = TRUE, warn = TRUE
) {
  #
  if (!is.null(select)) { # if selecting, no need to make an additional copy
    data <- data[, .SD, .SDcols = c(select)]
  } else if (copy) {      # otherwise, if copying, do so
    data <- as.data.table(data)
  }                       # otherwise, use data as-is

  if ((length(required_cols) > 0)) {     # if we have required columns ...
    if (!is.character(required_cols)) {  # ... check they are check-able
      stop("`required_cols` must be a character vector")
    }
    # check that all required columns are present
    if (!all(required_cols %in% colnames(dt))) {
      stop(
        msg_required,
        toString(required_cols[!(required_cols %in% colnames(dt))]),
        " but are not present among ",
        toString(colnames(dt)),
        "\n(all `required_cols`: ",
        toString(required_cols),
        ")"
      )
    }
  }

  if ((length(forbidden_cols) > 0)) {    # if we have forbidden columns ...
    if (!is.character(forbidden_cols)) { # ... check they are check-able
      stop("`forbidden_cols` must be a character vector")
    }
    # check that no forbidden columns are present
    if (any(forbidden_cols %in% colnames(dt))) {
      stop(
        msg_forbidden,
        toString(forbidden_cols[forbidden_cols %in% colnames(dt)]),
        " but are present among ",
        toString(colnames(dt)),
        "\n(all `forbidden_cols`: ",
        toString(forbidden_cols),
        ")"
      )
    }
  }

  if (length(select) > 0) {         # if selecting particular list ...
    return(dt[, .SD, .SDcols = c(select)][])
  } else {
    return(dt[])
  }

}
