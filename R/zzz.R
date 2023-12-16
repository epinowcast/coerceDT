
# default handling
internal_error <- function(..., call) stop(...)
internal_warn <- function(..., call) warning(...)

.onLoad <- function(libname, pkgname) {
  if (requireNamespace("cli", quietly = TRUE)) {
    utils::assignInMyNamespace(
      "internal_error",
      function(
        ..., call = parent.frame()
      ) {
        cli::cli_abort(message = c(...), call = call)
      }
    )
    utils::assignInMyNamespace(
      "internal_warn",
      function(
        ..., call = parent.frame()
      ) {
        cli::cli_warn(message = c(...), call = call)
      }
    )
  }
}

.onAttach <- function(libname, pkgname) {
  if (requireNamespace("cli", quietly = TRUE)) {
    packageStartupMessage("coerceDT: using `cli` messaging.")
  } else {
    packageStartupMessage("coerceDT: using `base` messaging; to use `cli` messages, install `cli`.")
  }
}
