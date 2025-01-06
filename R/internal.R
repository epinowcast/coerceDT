
#' @import data.table
NULL

#' Regularize `keep` argument
#'
#' @inheritParams castDT
#'
#' @return a generalized `keep` value: either an `integer()`, `character()`, or
#' named `list()`
#'
#' @keywords internal
.generalize_keep <- function(keep) {
  if (!(is.character(keep) || is.integer(keep) || is.list(keep))) {
    stop(sprintf(
      "`keep` = %s is not a `character`, `integer`, or `list`; found %s",
      toString(deparse(substitute(keep))), toString(class(keep))
    ))
  } else if (is.list(select)) {
    if (any(names(select) == "")) {
      stop("If a `list`, `select` must have `all(names(select) != '')`.")
    }
    select <- lapply(select, function(arg) {
      if (is.null(arg)) { # no-op transformation
        function(x) x
      } else if (is.function(arg)) {
        arg
      } else if (is.list(arg)) {
        # check at least one argument is a function, put it in first position
      } else {
        arg
      }
    })
  }
  return(select)
}

# anything that isn't a character will be thrown at as.data.table / setDT
# n.b.: keep / drop will be handled elsewhere; only copy relevant to this case
#' @rdname loadDT
#' @export
loadDT.default <- function(data, ..., copy = TRUE) {
  tryCatch(
    if (copy) as.data.table(data) else {
      if (is.list(data) || is.data.frame(data)) {
        setDT(data)
      } else {
        stop(sprintf("Cannot `copy=FALSE` when data is class %s", toString(class(data))), call. = FALSE)
      }
    },
    error = function(e) {
      stop(sprintf(
        "Failed to convert `data=` to a data.table; underlying error:\n  %s", e
      ))
    }
  )
}

# character case: either an rds or fed to fread
# n.b.: copy irrelevant here, since going to disk. keep / drop may be relevant
# if `fread`ing, otherwise handled later
#' @rdname loadDT
#' @export
loadDT.character <- function(data, keep, drop, ...) {
  if (length(data) > 1) {
    stop("`makeDT` does not support `data=` character vectors with length > 1")
  } else if (grepl("^[^[:space:]]\\.rds", data, ignore.case = TRUE)) {
    return(readRDS(data))
  } else {
    calllist <- list(input = data, data.table = TRUE)
    if (!missing(keep) && !missing(drop)) {
      stop("Use either `keep=` or `drop=`, but not both")
    }
    if (!missing(keep)) {
      if (is.list(keep)) {
        calllist$select <- names(keep)
      } else {
        callist$select <- keep
      }
    }
    if (!missing(drop)) {
      callist$drop <- drop
    }
    return(do.call(data.table::fread, calllist))
  }

}


#' @title Internal Method for Loading Data
#'
#' @inheritParams castDT
#'
#' @details
#' The `keep` argument must have already been [.generalize_keep()]'d.
#'
#' @keywords internal
#' @export
loadDT <- function(data, keep, drop, copy = TRUE) {
  UseMethod("loadDT")
}
