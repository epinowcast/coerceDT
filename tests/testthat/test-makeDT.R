
test_data <- function(filename) file.path(test_path(), "testdata", filename)

# files
test_rds <- test_data("simple.rds")
test_std <- as.data.table(readRDS(test_rds))

test_that("`makeDT` hands off correctly to `coerceDT` then `checkDT`", {
  expect_no_error(makeDT(test_std, select = c("x", "y"), forbidden = "z"))
})
