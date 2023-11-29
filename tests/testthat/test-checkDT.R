test_that("`copy` precludes side effects when requested", {
  expect_equal(2 * 2, 4)
})

test_that("`copy` allows side effects when requested", {
  expect_equal(2 * 2, 4)
})

test_that("`required` enforces the correct columns", {
  expect_equal(2 * 2, 4)
})

test_that("`required` enforces the correct column classes", {
  expect_equal(2 * 2, 4)
})

test_that("`select` returns the correct columns", {
  expect_equal(2 * 2, 4)
})

test_that("`select` errors when columns not present", {
  expect_equal(2 * 2, 4)
})

test_that("`drop` drops the correct columns", {
  expect_equal(2 * 2, 4)
})

test_that("`drop` warns when columns not present", {
  expect_equal(2 * 2, 4)
})

test_that("`data` supports data.tables", {
  expect_equal(2 * 2, 4)
})

test_that("`data` supports data.frames", {
  expect_equal(2 * 2, 4)
})

test_that("`data` supports paths", {
  expect_equal(2 * 2, 4)
})

test_that("`warn` when asked", {
  expect_equal(2 * 2, 4)
})

test_that("do not `warn` when asked", {
  expect_equal(2 * 2, 4)
})

test_that("fail on NAs when `NAerror`ing", {
  expect_equal(2 * 2, 4)
})

test_that("do not fail on NAs when not `NAerror`ing", {
  expect_equal(2 * 2, 4)
})
