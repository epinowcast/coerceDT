
test_data <- function(filename) file.path(test_path(), "testdata", filename)

# paths
test_csv <- test_data("simple.csv")
test_rds <- test_data("simple.rds")
# files
test_obj <- readRDS(test_rds)
test_std <- as.data.table(readRDS(test_rds))

allmodes <- list(csv = test_csv, rds = test_rds, obj = test_obj, ref = test_std)

# fread infos re using cmd interface implicitly; not sure what we can do about
# this *currently* - would take some fancy string munging to pass it to
# the correct fread parameter
test_that("`data` supports commands", {
  cmd <- "seq 1 5"
  expect_no_error(suppressMessages(coerceDT(cmd)))
})

######################################################
# test reading different data sources

test_that("`data` argument supports all modes", {
  lapply(allmodes, function(arg) {
    expect_no_error(res <- coerceDT(arg))
    expect_identical(res, test_std)
  })
})

test_that("`copy` precludes side effects when requested", {
  dforig <- readRDS(test_rds)
  dfref <- dforig
  dfmod <- coerceDT(dforig, copy = TRUE)
  dfmod[, a := 1L]
  expect_identical(dforig, dfref)
})

test_that("`copy` allows side effects when requested", {
  dforig <- readRDS(test_rds)
  dfmod <- coerceDT(dforig, copy = FALSE)
  expect_true(rlang::is_reference(dforig, dfmod))
  dfmod[, a := 1L]
  expect_true("a" %in% names(dforig))
})

test_that("`select` returns the correct columns + order for all modes", {
  cols <- c("y", "x")
  lapply(allmodes, function(arg) {
    expect_named(coerceDT(arg, select = cols), cols)
  })
})

test_that("`select` warns when columns not present", {
  lapply(allmodes, function(arg) {
    expect_warning(coerceDT(arg, select = "a"))
  })
})

test_that("`select` will convert columns", {
  selstmt <- list(
    x = "numeric",
    y = function(x) factor(x, levels = sort(unique(x)), ordered = TRUE)
  )
  lapply(allmodes, function(arg) {
    expect_no_error(res <- coerceDT(arg, select = selstmt))
    expect_identical(
      unname(res[, sapply(.SD, function(col) class(col)[1L])]),
      c("numeric", "ordered")
    )
  })
})

test_that("`select` will warn about coercion to NA", {
  selstmt <- list(x = "numeric", y = "integer")
  lapply(allmodes, function(arg) {
    expect_warning(res <- coerceDT(arg, select = selstmt))
    expect_true(all(is.na(res$y)))
  })
})

test_that("`drop` drops the correct columns", {
  dropcol <- "x"
  lapply(allmodes, function(arg) {
    expect_false(any(dropcol %in% names(coerceDT(arg, drop = dropcol))))
  })
})

test_that("`drop` warns when columns not present", {
  dropcol <- c("x", "zz")
  lapply(allmodes, function(arg) {
    expect_warning(coerceDT(arg, drop = dropcol))
  })
})

test_that("`default` does not overwrite columns when they are present.", {
  def <- list(x = 1, y = 2, z = 3)
  lapply(allmodes, function(arg) {
    expect_identical(test_std, coerceDT(arg, default = def))
  })
})

test_that("`default` creates columns when they are not present.", {
  def <- list(x = 1, y = 2, z = 3, a = "Z")
  test_ref <- copy(test_std)[, a := def$a ]
  lapply(allmodes, function(arg) {
    expect_identical(test_ref, coerceDT(arg, default = def))
  })
})
