
#' @title Internal Warnings and Errors
#'
#' @description
#' Manage internal warnings and errors to either use `base` or `cli` messaging.
#'
#' @param ... either [base::stop()] or [base::warning()] `...` arguments or
#' [cli::cli_abort()] or [cli::cli_warn()] `message` contents.
#' @param call the calling environment, used in error messages
#'
#' @return NULL
#' 
#' @keywords internal
internal_error <- function(..., call = parent.frame()) {
  if (getOption("makeDT.cli")) {
    cli::cli_abort(message = c(...), call = call)
  } else {
    stop(...)
  }
}

#' @rdname internal_error
#' @keywords internal
internal_warn <- function(..., call = parent.frame()) {
  if (getOption("makeDT.cli")) {
    cli::cli_warn(message = c(...), call = call)
  } else {
    warning(...)
  }
}

.onLoad <- function(libname, pkgname) {
  cli_available <- requireNamespace("cli", quietly = TRUE)
  use_cli <- getOption("makeDT.cli", default = cli_available)
  if (use_cli && !cli_available) {
    warning(
      "`cli` requested but not available; using `base` messaging.\n",
      "Either install `cli` or check your `makeDT.cli` option."
    )
    use_cli <- FALSE
  }
  options(makeDT.cli = use_cli)
}

.onAttach <- function(libname, pkgname) {
  if (getOption("makeDT.cli")) {
    packageStartupMessage("coerceDT: using `cli` messaging.")
  } else {
    packageStartupMessage(
      paste(c(
        "coerceDT: using `base` messaging.",
        "To use `cli` messages, install `cli` and restart your session,",
        "or check your `options()$makeDT.cli` value."
      ), collapse = "\n\t")
    )
  }
}
