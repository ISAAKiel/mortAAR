test_that("lt.correction failes for wrong input", {
  expect_snapshot_error(lt.representativity(1))
})

test_that("lt.correction produces the right output", {
  sl <- life.table(schleswig_ma[c("a", "Dx")])
  expect_snapshot_value(
    lt.correction(sl),
    style = c("json2")
  )
  expect_snapshot_value(
    lt.correction(sl, agecor = FALSE),
    style = c("json2")
  )
  od <- life.table(list(
    "sl1" = schleswig_ma[c("a", "Dx")],
    "sl2" = schleswig_ma[c("a", "Dx")]
  ))
  expect_snapshot_value(
    lt.correction(od),
    style = c("json2")
  )
})
