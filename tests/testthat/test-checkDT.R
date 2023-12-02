
test_data <- function(filename) file.path(test_path(), "testdata", filename)

# files
test_rds <- test_data("simple.rds")
test_std <- as.data.table(readRDS(test_rds))

test_that("`required` columns does not error when columns present", {
  expect_no_error(checkDT(test_std, required = c("x", "y")))
})

test_that("`required` columns errors when columns not present", {
  expect_error(checkDT(test_std, required = c("x", "a")))
})

test_that("`required` column checks do not error when passing", {
  reqstmt <- list(x = "integer", y = function(x) x %in% LETTERS)
  expect_no_error(checkDT(test_std, required = reqstmt))
})

test_that("`required` column checks error when not passing", {
  reqstmt <- list(x = "integer", y = function(x) x %in% letters)
  expect_error(checkDT(test_std, required = reqstmt))
})

test_that("`forbidden` columns does not error when columns not present", {
  expect_no_error(checkDT(test_std, forbidden = c("a", "b")))
})

test_that("`forbidden` columns errors when columns are present", {
  expect_error(checkDT(test_std, forbidden = c("x", "a")))
})
