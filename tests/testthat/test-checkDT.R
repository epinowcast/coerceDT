
test_data <- function(filename) file.path(test_path(), "testdata", filename)

# files
test_rds <- test_data("simple.rds")
test_std <- as.data.table(readRDS(test_rds))

test_that("`require` columns does not error when columns present", {
  expect_no_error(checkDT(test_std, require = c("x", "y")))
})

test_that("`require` columns errors when columns not present", {
  expect_error(checkDT(test_std, require = c("x", "a")))
})

test_that("`require` column checks do not error when passing", {
  reqstmt <- list(x = "integer", y = function(x) x %in% LETTERS)
  expect_no_error(checkDT(test_std, require = reqstmt))
})

test_that("`require` column checks error when not passing", {
  reqstmt <- list(x = "integer", y = function(x) x %in% letters)
  expect_error(checkDT(test_std, require = reqstmt))
})

test_that("`forbid` columns does not error when columns not present", {
  expect_no_error(checkDT(test_std, forbid = c("a", "b")))
})

test_that("`forbid` columns errors when columns are present", {
  expect_error(checkDT(test_std, forbid = c("x", "a")))
})
