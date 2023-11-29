
#' @param dt a `data.table`: the data
#' @param reqs a `list` or `character` vector: the requirements
requiring <- function(dt, reqs) {
  names_ <- names(reqs)
  # if there are no names, entries are just columns that need to appear
  if (is.null(names_)) {
    found_ <- reqs %in% names(dt)
    if (any(!found_)) {
      stop(sprintf(
        "coerceDT: required column(s) (%s) not found among (%s).",
        toString(reqs[!found_]),
        toString(names(dt))
      ))
    }
  } else { # otherwise, there exist some names, but may be ""s

  }
}
