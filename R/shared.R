
report_error <- function(bad_columns, error_msg, call = parent.frame()) {
  if (length(bad_columns) != 0L) {
    internal_error(sprintf(error_msg, toString(bad_columns)), call = call)
  }
}
