
testthat("`cli` is used if available.", {
  warntext <- c("something", "to", "examine")
  options(makeDT.cli = TRUE)
  expect_warning(
    internal_warn(warntext),
    regexp = paste0(warntext, collapse = ".+")
  )
  options(makeDT.cli = FALSE)
  expect_warning(
    internal_warn(warntext),
    regexp = paste0(warntext, collapse = "")
  )
})