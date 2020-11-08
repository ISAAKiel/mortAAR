test_that("lt.sexrelation failes for wrong input", {
  expect_snapshot_error(lt.sexrelation(1))
})

test_that("lt.sexrelation produces the right output", {
  nitra_prep <- prep.life.table(nitra, group="sex", agebeg = "age_start", ageend = "age_end")
  nt <- life.table(nitra_prep)
  expect_snapshot_value(
    lt.sexrelation(nt$f, nt$m),
    style = c("json2")
  )
})

test_that("lt.sexrelation failes for wrong second input element", {
    nitra_prep <- prep.life.table(nitra, group="sex", agebeg = "age_start", ageend = "age_end")
    nt <- life.table(nitra_prep)
    er <- 2
    expect_snapshot_error(
      lt.sexrelation(nt$f, er)
    )
})
