test_that("lt.reproduction fails for wrong input", {
  expect_snapshot_error(lt.reproduction(1))
})

test_that("lt.reproduction produces the right output", {
  sl <- life.table(schleswig_ma[c("a", "Dx")])
  expect_snapshot_value(
    lt.reproduction(sl, fertility_rate = "McFO"),
    style = c("json2")
  )
  expect_snapshot_value(
    lt.reproduction(sl, fertility_rate = "BA_linear"),
    style = c("json2")
  )
  expect_snapshot_value(
    lt.reproduction(sl, fertility_rate = "BA_power"),
    style = c("json2")
  )
  expect_snapshot_value(
    lt.reproduction(sl, fertility_rate = "BA_log"),
    style = c("json2")
  )
  expect_snapshot_value(
    lt.reproduction(sl, fertility_rate = 5),
    style = c("json2")
  )
  expect_snapshot_value(
    lt.reproduction(sl, growth_rate = "fertility"),
    style = c("json2")
  )
  expect_snapshot_value(
    lt.reproduction(sl, growth_rate = "MBA"),
    style = c("json2")
  )
    expect_snapshot_value(
    lt.reproduction(sl, growth_rate = "McFO"),
    style = c("json2")
  )
    expect_snapshot_value(
    lt.reproduction(sl,growth_rate = 1.1),
    style = c("json2")
  )
  expect_snapshot_value(
    lt.reproduction(sl, gen_len = 25),
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
