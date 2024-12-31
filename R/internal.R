
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
      deparse(substitute(keep)), toString(class(keep))
    ))
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

#' @title Internal Method for Loading Data
#'
#' @inheritParams castDT
#'
#' @details
#' The `keep` argument must have already been [.generalize_keep()]'d.
#'
#' @keywords internal
.loadDT <- function(data, keep, drop, copy) {
  UseMethod("loadDT")
}

# n.b.: keep / drop will be handled elsewhere; only copy relevant here
# anything that isn't a character will be thrown at as.data.table / setDT
#' @rdname loadDT
.loadDT.default <- function(data, ..., copy) {
  tryCatch(
    if (copy) as.data.table(data) else setDT(data),
    error = function(e) {
      stop(sprintf(
        "Failed to convert `data=%s` to a data.table; underlying error: %s",
        deparse(substitute(data)), e
      ))
    }
  )
}

# n.b.: copy irrelevant here, since going to disk. keep / drop may be relevant
# if `fread`ing
#' @rdname loadDT
.loadDT.character <- function(data, keep, drop, ...) {
  if (grepl("^[^[:space:]]\\.rds", data, ignore.case = TRUE)) {
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
