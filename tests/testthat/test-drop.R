
test_that("`drop`ped columns are not present & non-`drop`ped are present.", {
  obj <- get_a("obj")

  allcol <- names(obj)
  dropcol <- allcol[-1]
  cast_dt <- castDT(obj, drop = dropcol)
  expect_false(any(dropcol %in% names(cast_dt)))

  nondrop <- dropcol
  dropcol <- allcol[1]
  cast_dt <- castDT(obj, drop = dropcol)
  expect_true(all(nondrop %in% names(cast_dt)))
})

test_that("`drop` does not cause a warning when targets not present.", {
  dropcol <- "notacolumn"
  expect_no_condition(cast(test_obj, drop = dropcol))
})

test_that("`drop` has / does not have side effects as requested.", {
  obj <- get_a("obj")
  allcol <- names(obj)
  dropcol <- allcol[1]
  objA_dt <- cast_dt(obj, drop = dropcol)
  expect_equal(allcol, names(obj))
  objB_dt <- cast_dt(objA, drop = dropcol, copy = FALSE)
  expect_equal(names(objB_dt), names(obj))
})
