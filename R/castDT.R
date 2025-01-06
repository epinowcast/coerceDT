#' @title Convert to `data.table`
#'
#' @description Provides consistent casting of `data` to [data.table]
#' with error handling.
#'
#' @param data Any of the types supported by [data.table::as.data.table()] OR
#' a single character string. If a string, checked against a case-insensitive
#' pattern `^[^ ]+\\.rds$` (i.e. a path with no spaces and ending with ".rds").
#' If matched, uses [readRDS()], otherwise uses [data.table::fread()].
#'
#' @param keep columns to select; others dropped. May be: a `character()`,
#' an `integer()` or a named `list()`. If a named list, the values in the list
#' can provide default values and/or casting operations.
#'  - if the value is `NULL`, no default or casting; i.e. this is a column to
#'  keep, but there are no transformations to apply or defaults to set.
#'  - if the value satisfies [is.function()]: just a transformation
#'  - if the value satisfies [is.list()]: both a transformation and default
#'  value
#'  - otherwise: the default value that will be set if the column is not present
#'
#' @param drop columns to drop; others selected. May be: a `character()` or
#' `integer()`. Emits a warning if dropped columns are not present.
#'
#' @param copy Logical; if `TRUE` (default), a new `data.table` is returned;
#' if `FALSE`, `data` will be modified in place if already an R object.
#'
#' @return A `data.table`; the returned object will be a copy (default), unless
#' `copy = FALSE`, in which case modifications are made in-place for extant
#' objects (i.e. if `data` is not a character)
#'
#' @details This function provides a general-purpose tool for basic conversion
#' operations with `data.table`s. It's intended use is as a simplifying,
#' standardizing interface for data import and column-wise manipulations.
#'
#' Note: this function will emit any warnings or errors from `keep` argument
#' casting functions.
#'
#' @examples
#' mtdt <- castDT(mtcars)
#' mtdt2 <- castDT(mtdt, keep = c("disp", "hp"))
#' mtdt3 <- castDT(mtdt, keep = 3:4)
#' # same as previous
#' all(mtdt2 == mtdt3)
#'
#' @importFrom data.table setDT as.data.table
#'
#' @export
castDT <- function(
  data,
  keep, drop,
  copy = TRUE
) {

  if (!missing(keep) && !missing(drop)) {
    stop("Use either select= or drop= but not both")
  }

  .doargs <- as.list(environment())
  if (!missing(keep)) {
    .doargs$keep <- .generalize_keep(keep)
  }

  data <- do.call(loadDT, .doargs)

  if (!missing(keep)) {

  }

  if (!missing(drop)) {
    for (nm in drop) data[[nm]] <- NULL
  }

}



#' Regularize `default` argument
#'
#' @inheritParams castDT
#'
#' @return a checked `default` list.
check_default <- function(default) {
  default <- as.list(default)
  if (any(names(default) == "")) {
    stop("`default` must have `all(names(default) != '')`.")
  }
  return(default)
}

coerce_select <- function(data, select, selnames = names(select)) {
  data[,
       c(selnames) := mapply(
         function(f, col) f(.SD[[col]]),
         f = select, col = selnames, SIMPLIFY = FALSE
       ),
       .SDcols = selnames
  ]
}

coerce_default <- function(data, default) {
  for (defcol in names(default)) {
    # if not present, create with default
    if (is.null(data[[defcol]])) {
      data[, c(defcol) := default[defcol]]
    }
  }
  data
}

#' Select, drop, and convert columns
#'
#' @param data a `data.table`
#'
#' @inheritParams castDT
#'
#' @details
#' ALWAYS modifies `data` in place. Does NOT check for consistency of `select`
#' and `drop` arguments (i.e. at most one non missing)
#'
#' @importFrom data.table setcolorder
internal_select_drop_convert <- function(
  data,
  select, drop, default
) {

  # first, consider default columns:
  #  - they will created in the data if not present
  #  - they will added to select columns, if selecting
  if (!missing(default)) {
    coerce_default(data, default)
    default <- lapply(default, function(d) NULL)
    # if there is an explicit select, add all defaults to it
    if (!missing(select)) {
      if (is.character(select)) {
        select <- unique(c(select, names(default)))
      } else if (is.integer(select)) {
        select <- c(select, dim(data)[2L] + (1L - length(default)):0L)
      } else {
        for (defcol in names(default)) {
          if (is.null(select[defcol])) select[defcol] <- default[defcol]
        }
      }
    }
  }

  # now if selecting, convert & use selection
  if (!missing(select)) {
    if (is.character(select)) {
      selnames <- select
    } else if (is.integer(select)) {
      selnames <- names(data)[select]
    } else {
      selnames <- names(select)
    }
    if (missing(drop)) {
      # null everything that isn't in select
      drop <- setdiff(names(data), selnames)
    }
    selord <- intersect(selnames, names(data))
    # warn for non-present items
    if (length(select) != length(selord)) {
      warning("Some cols not present")
    }
    setcolorder(data, selord)
    if (is.list(select)) {
      coerce_select(data, select, selnames)
    }
  }

  if (!missing(drop) && length(drop) > 0L) {
    # null everything in drop
    # will warn if non-present items
    data[, c(drop) := NULL]
  }

  data
}
