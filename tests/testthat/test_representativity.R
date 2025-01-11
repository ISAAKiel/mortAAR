test_that("lt.representativity fails for wrong input", {
  expect_snapshot_error(lt.representativity(1))
})

test_that("lt.representativity produces the right output", {
  sl <- life.table(schleswig_ma[c("a", "Dx")])
  expect_snapshot_value(
    lt.representativity(sl),
    style = c("json2")
  )
})
