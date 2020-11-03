test_that("life.table sexrelation does the mathing correctly", {
  # dx
  expect_true(
    dplyr::near(
      single_life_table$dx,
      schleswig_ma$dx/10,
      tol = 0.1
    ) %>% all
  )
  # lx
  expect_true(
    dplyr::near(
      single_life_table$lx,
      schleswig_ma$lx/10,
      tol = 0.1
    ) %>% all
  )
})
