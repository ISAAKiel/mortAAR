test_that("lt.reproduction failes for wrong input", {
  expect_snapshot_error(lt.reproduction(1))
})

test_that("lt.reproduction produces the right output", {
  sl <- life.table(schleswig_ma[c("a", "Dx")])
  expect_snapshot_value(
    lt.reproduction(sl),
    style = c("json2")
  )
  od <- life.table(list(
    "corpus mandibulae" = odagsen_cm[c("a", "Dx")],
    "margo orbitalis" = odagsen_mo[c("a", "Dx")]
  ))
  expect_snapshot_value(
    lt.reproduction(od),
    style = c("json2")
  )
})
