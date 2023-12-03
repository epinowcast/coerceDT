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
#' `coerceDT` will error if any of those columns are present. If anything else,
#' will error.
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
#' @importFrom data.table is.data.table
#' @export
checkDT <- function(
  data,
  required = NULL, forbidden = NULL
) {
  if (!is.data.table(data)) stop(
    "`data` must be a data.table; perhaps `coerceDT()` first?"
  )
  if (!is.null(required)) {
    required <- check_required(required)
    if (is.character(required)) {
      if (!all(required %in% names(data))) {
        stop("`data` does not contain `required` columns.")
      }
    } else if (is.list(required)) {
      cols <- names(required)
      if (!all(cols %in% names(data))) {
        stop("`data` does not contain `required` columns.")
      }
      failed <- data[,
         cols[!mapply(
           function(f, col) all(f(.SD[[col]])),
           f = required, col = cols, SIMPLIFY = TRUE
         )],
         .SDcols = cols
      ]
      if (length(failed) != 0) {
        stop("`required` some column did not pass.")
      }
    }
  }
  if (!is.null(forbidden)) {
    if (!is.character(forbidden)) stop("`forbidden` must be a `character`")
    if (any(forbidden %in% names(data))) {
      stop("`data` contains `forbidden` columns.")
    }
  }
  data
}

check_required <- function(required) {
  if (!(is.character(required) || is.list(required))) {
    stop("`required` is not a `character` or named `list`")
  } else if (is.list(required)) {
    if (is.null(names(required)) || any(names(required) == "")) {
      stop("If a `list`, `required` must have `all(names(required) != '')`.")
    }
    required <- lapply(required, function(arg) {
      if (is.null(arg)) {
        function(x) TRUE
      } else if (is.character(arg) && length(arg) == 1) {
        get(paste("is", arg, sep = "."))
      } else if (is.function(arg)) {
        arg
      } else stop(
        "If a `list`, `required` must specify checks, either",
        "as NULL (no check other than presence),",
        "a string (is.TYPE check),",
        "or a function (f(x) check)"
      )
    })
  }
  return(required)
}
