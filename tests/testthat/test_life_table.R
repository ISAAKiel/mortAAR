test_that("life.table fails for wrong input", {
  expect_snapshot_error(life.table(1))
})

test_that("life.table produces the right output with spline-function", {
  nitra_prep <- prep.life.table(nitra, method="Equal5",
                                agebeg = "age_start", ageend = "age_end")
  expect_snapshot_value(
    life.table(nitra_prep,option_spline = 10),
    style = c("serialize")
    # style needs to be 'serialize' (and thus completely opaque) because
    # there seem to be rounding errors with 'json2'
  )
})
