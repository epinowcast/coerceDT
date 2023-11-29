
test_data <- function(filename) file.path(test_path(), "testdata", filename)

test_that("`copy` precludes side effects when requested", {
  dforig <- readRDS(test_data("simple.rds"))
  dfref <- dforig
  dfmod <- coerceDT(dforig, copy = TRUE)
  dfmod[, z := 0]
  expect_equal(dforig, dfref)
})

test_that("`copy` allows side effects when requested", {
  dforig <- readRDS(test_data("simple.rds"))
  dfmod <- coerceDT(setDT(dforig), copy = FALSE)
  expect_true(rlang::is_reference(dforig, dfmod))
})

test_that("`select` returns the correct columns", {
  cols <- c("y", "x")
  res <- coerceDT(test_data("simple.csv"), select = cols)
  expect_equal(names(res), cols)
  res <- coerceDT(test_data("simple.rds"), select = cols)
  expect_equal(names(res), cols)
  orig <- readRDS(test_data("simple.rds"))
  res <- coerceDT(orig, select = cols)
  expect_equal(cols, names(res))
})

test_that("`select` warns when columns not present", {
  res <- readRDS(test_data("simple.rds"))
  expect_warning(coerceDT(res, select = "a"))
})

test_that("`drop` drops the correct columns", {
  dropcol <- "x"
  res <- readRDS(test_data("simple.rds"))
  expect_false(any(dropcol %in% names(coerceDT(res, drop = dropcol))))
})

test_that("`drop` warns when columns not present", {
  dropcol <- c("x", "zz")
  res <- readRDS(test_data("simple.rds"))
  expect_warning(coerceDT(res, drop = dropcol))
})

test_that("`data` supports data.tables", {
  orig <- setDT(readRDS(test_data("simple.rds")))
  expect_no_error(coerceDT(orig))
  expect_no_error(coerceDT(orig, copy = FALSE))
})

test_that("`data` supports data.frames", {
  orig <- readRDS(test_data("simple.rds"))
  expect_no_error(coerceDT(orig))
  expect_no_error(coerceDT(orig, copy = FALSE))
})

test_that("`data` supports rds paths", {
  orig <- test_data("simple.rds")
  expect_no_error(coerceDT(orig))
  expect_no_error(coerceDT(orig, copy = FALSE))
})

test_that("`data` supports csv paths", {
  orig <- test_data("simple.csv")
  expect_no_error(coerceDT(orig))
  expect_no_error(coerceDT(orig, copy = FALSE))
})

# fread infos re using cmd interface implicitly; not sure what we can do about
# this *currently* - would take some fancy string munging to pass it to
# the correct fread parameter
test_that("`data` supports commands", {
  cmd <- "seq 1 5"
  expect_no_error(suppressMessages(coerceDT(cmd)))
  expect_no_error(suppressMessages(coerceDT(cmd, copy = FALSE)))
})
