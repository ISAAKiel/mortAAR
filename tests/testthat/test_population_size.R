test_that("lt.population_size fails for wrong input", {
  expect_snapshot_error(lt.population_size(1))
})

test_that("lt.population_size produces the right output", {
  sl <- life.table(schleswig_ma[c("a", "Dx")])
  expect_snapshot_value(
    lt.population_size(sl, t = 100),
    style = c("json2")
  )
  od <- life.table(list(
    "corpus mandibulae" = odagsen_cm[c("a", "Dx")],
    "margo orbitalis" = odagsen_mo[c("a", "Dx")]
  ))
  expect_snapshot_value(
    lt.population_size(od, t = 50),
    style = c("json2")
  )
  expect_snapshot_value(
    lt.population_size(x = 111, e0 = 32.2, k = 1.2, t = 100),
    style = c("json2")
  )
})
