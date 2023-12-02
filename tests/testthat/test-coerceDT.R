
test_data <- function(filename) file.path(test_path(), "testdata", filename)

test_csv <- test_data("simple.csv")
test_rds <- test_data("simple.rds")
test_std <- as.data.table(readRDS(test_rds))

######################################################
# test reading different data sources

test_that("`data` supports data.frames", {
  orig <- readRDS(test_rds)
  expect_no_error(res <- coerceDT(orig))
  expect_equal(res, test_std)
})

test_that("`data` supports rds paths", {
  expect_no_error(res <- coerceDT(test_rds))
  expect_equal(res, test_std)
})

test_that("`data` supports csv paths", {
  expect_no_error(res <- coerceDT(test_csv))
  expect_equal(res, test_std)
})

#

test_that("`copy` precludes side effects when requested", {
  dforig <- readRDS(test_rds)
  dfref <- dforig
  dfmod <- coerceDT(dforig, copy = TRUE)
  dfmod[, a := 1]
  expect_equal(dforig, dfref)
})

test_that("`copy` allows side effects when requested", {
  dforig <- readRDS(test_rds)
  dfmod <- coerceDT(dforig, copy = FALSE)
  expect_true(rlang::is_reference(dforig, dfmod))
})

test_that("`select` returns the correct columns", {
  cols <- c("y", "x")
  res <- coerceDT(test_csv, select = cols)
  expect_equal(names(res), cols)
  res <- coerceDT(test_rds, select = cols)
  expect_equal(names(res), cols)
  orig <- readRDS(test_rds)
  res <- coerceDT(orig, select = cols)
  expect_equal(cols, names(res))
})

test_that("`select` warns when columns not present", {
  res <- readRDS(test_rds)
  expect_warning(coerceDT(res, select = "a"))
})

test_that("`drop` drops the correct columns", {
  dropcol <- "x"
  res <- readRDS(test_rds)
  expect_false(any(dropcol %in% names(coerceDT(res, drop = dropcol))))
})

test_that("`drop` warns when columns not present", {
  dropcol <- c("x", "zz")
  res <- readRDS(test_rds)
  expect_warning(coerceDT(res, drop = dropcol))
})

test_that("`data` supports data.tables", {
  orig <- data.table::setDT(readRDS(test_rds))
  expect_no_error(coerceDT(orig))
  expect_no_error(coerceDT(orig, copy = FALSE))
})

######################################################
# test different data sources


test_that("`data` supports data.frames", {
  orig <- readRDS(test_rds)
  expect_no_error(coerceDT(orig))
})

test_that("`data` supports rds paths", {
  expect_no_error(coerceDT(test_rds))
})

test_that("`data` supports csv paths", {
  expect_no_error(coerceDT(test_csv))
})

# fread infos re using cmd interface implicitly; not sure what we can do about
# this *currently* - would take some fancy string munging to pass it to
# the correct fread parameter
test_that("`data` supports commands", {
  cmd <- "seq 1 5"
  expect_no_error(suppressMessages(coerceDT(cmd)))
})
