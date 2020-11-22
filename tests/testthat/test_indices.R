test_that("lt.indices fails for wrong input", {
  expect_snapshot_error(lt.indices(1))
})

test_that("lt.indices produces the right output", {
  sl <- life.table(schleswig_ma[c("a", "Dx")])
  expect_snapshot_value(
    round_5_xs(lt.indices(sl)),
    style = c("json2")
  )
  od <- life.table(list(
    "corpus mandibulae" = odagsen_cm[c("a", "Dx")],
    "margo orbitalis" = odagsen_mo[c("a", "Dx")]
  ))
  expect_snapshot_value(
    round_5_xss(lt.indices(od)),
    style = c("json2")
  )
})

test_that("helper function lt.mortality produces the right output", {
  sl <- life.table(schleswig_ma[c("a", "Dx")])
  expect_snapshot_value(
    round_5_xs(lt.mortality(sl)),
    style = c("json2")
  )
})
