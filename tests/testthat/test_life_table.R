test_that("life.table fails for wrong input", {
  expect_snapshot_error(life.table(1))
})

test_that("life.table produces the right output with spline-function", {
  magdalenenberg <- magdalenenberg %>% replace(magdalenenberg == "60-x", "60-69")
  magdalenenberg <- magdalenenberg %>% tidyr::separate(a, c("from", "to")) %>%
    transform(from = as.numeric(from), to = as.numeric(to))
  mag_prep <- magdalenenberg %>%
    prep.life.table(
      dec = "Dx",
      agebeg = "from",
      ageend = "to",
      method = "Equal5",
      agerange = "included"
    )
  expect_snapshot_value(
    life.table(mag_prep,option_spline = 10),
    style = c("serialize")
    # style needs to be 'serialize' (and thus completely opaque) because
    # there seem to be rounding errors with 'json2'
  )
})
