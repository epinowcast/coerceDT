#' @title Check `data.table`s
#'
#' @description Provides a checking interface for `data.table`s
#'
#' @param data a `data.table`, e.g. as a product of [coerceDT()]
#'
#' @param require Optional; if `NULL` (the default), there are no required
#' columns. If a `character` vector, `coerceDT` will produce an error indicating
#' which columns are not present. If a named `list`, the names will be required
#' columns and the `list` entries will be used to check the corresponding
#' columns. If those entries are themselves characters, those will be assumed to
#' be the class to check to via an `is.Class` method; otherwise, they should be
#' single argument functions that will be used to transform the column.
#'
#' @param forbid Optional; if `NULL`, ignored. If a character vector,
#' `coerceDT` will error if any of those columns are present. If anything else,
#' will error.
#'
#' @return `data` itself, assuming passing require and forbid
#'
#' @details This function provides a general-purpose tool for common, basic
#' checking and conversion tasks with `data.table`s. It's intended use is as
#' a simplifying, standardizing interface for raw input checking, not to perform
#' complex requirement checks or manipulations. It is not, e.g., able to answer
#' if one-and-only-one of some set of columns are present, or to coerce column
#' values to a new values based on anything other than their initial value.
#'
#' @importFrom data.table is.data.table
#' @export
checkDT <- function(
  data,
  require = NULL, forbid = NULL
) {
  if (!is.data.table(data)) internal_error(
    "`data` must be a data.table; perhaps `coerceDT()` first?"
  )
  if (!is.null(require)) {
    require <- check_required(require)
    if (is.character(require)) {
      if (!all(require %in% names(data))) {
        internal_error("`data` does not contain `require` columns.")
      }
    } else if (is.list(require)) {
      cols <- names(require)
      if (!all(cols %in% names(data))) {
        internal_error("`data` does not contain `require` columns.")
      }
      failed <- data[,
         cols[!mapply(
           function(f, col) all(f(.SD[[col]])),
           f = require, col = cols, SIMPLIFY = TRUE
         )],
         .SDcols = cols
      ]
      if (length(failed) != 0L) {
        internal_error("`require` some column did not pass.")
      }
    }
  }
  if (!is.null(forbid)) {
    if (!is.character(forbid)) internal_error("`forbid` must be a `character`")
    if (any(forbid %in% names(data))) {
      internal_error("`data` contains `forbid` columns.")
    }
  }
  data
}

check_required <- function(require, call = parent.frame()) {
  if (!(is.character(require) || is.list(require))) {
    internal_error("`require` is not a `character` or named `list`", call = call)
  } else if (is.list(require)) {
    if (is.null(names(require)) || any(names(require) == "")) {
      internal_error("If a `list`, `require` must have `all(names(require) != '')`.", call = call)
    }
    require <- lapply(require, function(arg) {
      if (is.null(arg)) {
        function(x) TRUE
      } else if (is.character(arg) && length(arg) == 1L) {
        get(paste("is", arg, sep = "."))
      } else if (is.function(arg)) {
        arg
      } else {
        internal_error(
          "If a `list`, `require` must specify checks, either",
          "as NULL (no check other than presence),",
          "a string (is.TYPE check),",
          "or a function (f(x) check)",
          call = call
        )
      }
    })
  }
  return(require)
}
