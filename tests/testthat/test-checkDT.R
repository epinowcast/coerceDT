
test_data <- function(filename) file.path(test_path(), "testdata", filename)

# files
test_rds <- test_data("simple.rds")
test_std <- as.data.table(readRDS(test_rds))

test_that("`expect` columns does not error when columns present", {
  expect_no_error(checkDT(test_std, expect = c("x", "y")))
})

test_that("`expect` columns errors when columns not present", {
  expect_error(checkDT(test_std, expect = c("x", "a")))
})

test_that("`expect` column checks do not error when passing", {
  reqstmt <- list(x = "integer", y = function(x) x %in% LETTERS)
  expect_no_error(checkDT(test_std, expect = reqstmt))
})

test_that("`expect` column checks error when not passing", {
  reqstmt <- list(x = "integer", y = function(x) x %in% letters)
  expect_error(checkDT(test_std, expect = reqstmt))
})

test_that("`forbid` columns does not error when columns not present", {
  expect_no_error(checkDT(test_std, forbid = c("a", "b")))
})

test_that("`forbid` columns errors when columns are present", {
  expect_error(checkDT(test_std, forbid = c("x", "a")))
})
