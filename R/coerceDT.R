#' @title Coerce `data.table`s
#'
#' @description Provides consistent coercion of `data` to [data.table]
#' with error handling.
#'
#' @param data Any of the types supported by [data.table::as.data.table()] OR
#' a single character string. If a string, checked against pattern `\\.rds$`;
#' if match, uses [readRDS()], otherwise uses [data.table::fread()].
#'
#' @param select columns to select; others dropped. May be: a `character()`,
#' an `integer()` or a named `list()`. If a named list, the elements in the list
#' represent coercion operations for the columns: either NULL (no change),
#' a string (coerce via `as.TYPE`, where that function is present in the global
#' environment), or a function (coerce via `f(s)`).
#'
#' Emits a warning if selected elements are not present or if any column-wise
#' coercion would result in `NA` values.
#'
#' @param drop columns to drop; others selected. May be: a `character()` or
#' `integer()`. Emits a warning if dropped columns are not present.
#'
#' @param copy Logical; if `TRUE` (default), a new `data.table` is returned;
#' if `FALSE`, `data` will be modified in place if already an R object
#'
#' @return A `data.table`; the returned object will be a copy (default), unless
#' `copy = FALSE`, in which case modifications are made in-place for extant
#' objects (i.e. if `data` is not a character)
#'
#' @details This function provides a general-purpose tool for basic conversion
#' operations with `data.table`s. It's intended use is as a simplifying,
#' standardizing interface for data import and column-wise manipulations.
#'
#' @examples
#' mtdt <- coerceDT(mtcars)
#' mtdt2 <- coerceDT(mtdt, select = c("disp", "hp"))
#' mtdt3 <- coerceDT(mtdt, select = 3:4)
#' # same as previous
#' all(mtdt2 == mtdt3)
#'
#' @importFrom data.table setDT as.data.table
#'
#' @export
coerceDT <- function(
  data, select, drop, copy = TRUE
) {

  if (!missing(select) && !missing(drop)) {
    stop("Use either select= or drop= but not both")
  }

  doargs <- list()
  if (!missing(select)) {
    doargs$select <- check_select(select)
  } else if (!missing(drop)) {
    doargs$drop <- drop
  }

  if (is.character(data)) {
    if (grepl(pattern = "\\.rds$", x = data, ignore.case = TRUE)) {
      doargs$data <- readRDS(
        tryCatch(normalizePath(data), warning = function(e) stop(e))
      )
      doargs$copy <- FALSE
      do.call(coerceDT, doargs)
    } else {
      doargs$input <- data
      if (!missing(select) && is.list(select)) {
        selcoerce <- doargs$select
        doargs$select <- names(select)
        coerce_select(
          do.call(data.table::fread, doargs), selcoerce, doargs$select
        )
      } else {
        do.call(data.table::fread, doargs)
      }
    }
  } else {
    doargs$data <- if (copy)
      as.data.table(data)
    else
      eval(substitute(setDT(data)), parent.frame())
    do.call(internal_select_drop_convert, doargs)
  }
}

#' Regularize `select` argument
#'
#' @inheritParams coerceDT
#'
#' @return a checked `select` value; if `select` is a list, will have converted
#' all the elements to function calls.
check_select <- function(select) {
  if (!(is.character(select) || is.integer(select) || is.list(select))) {
    stop("`select` is not a `character`, `integer`, or `list`")
  } else if (is.list(select)) {
    if (any(names(select) == "")) {
      stop("If a `list`, `select` must have `all(names(select) != '')`.")
    }
    select <- lapply(select, function(arg) {
      if (is.null(arg)) {
        function(x) x
      } else if (is.character(arg) && length(arg) == 1L) {
        get(paste("as", arg, sep = "."))
      } else if (is.function(arg)) {
        arg
      } else {
        stop(
          "If a `list`, `select` must specify conversions, either",
          "as NULL (no conversion),",
          "a string (as.TYPE conversion),",
          "or a function (f(x) conversion)"
        )
      }
    })
  }
  return(select)
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

#' Select, drop, and convert columns
#'
#' @param data a `data.table`
#'
#' @inheritParams coerceDT
#'
#' @details
#' ALWAYS modifies `data` in place. Does NOT check for consistency of `select`
#' and `drop` arguments (i.e. at most one non missing)
#'
#' @importFrom data.table setcolorder
internal_select_drop_convert <- function(
  data,
  select, drop
) {

  if (!missing(select)) {
    if (is.character(select)) {
      selnames <- select
    } else if (is.integer(select)) {
      selnames <- names(data)[select]
    } else {
      selnames <- names(select)
    }
    # null everything that isn't in select
    drop <- setdiff(names(data), selnames)
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
