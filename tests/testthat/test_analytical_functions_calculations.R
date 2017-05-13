context("life.table calculations")

test_that("life.table does the mathing correctly", {
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
  # qx
  expect_true(
    dplyr::near(
      single_life_table$qx,
      schleswig_ma$qx/10,
      tol = 0.1
    ) %>% all
  )
  # Lx
  expect_true(
    dplyr::near(
      single_life_table$Lx,
      schleswig_ma$Lx/10,
      tol = 0.1
    ) %>% all
  )
  # Tx
  expect_true(
    dplyr::near(
      single_life_table$Tx,
      schleswig_ma$Tx/10,
      # cumulated error here already bigger
      tol = 0.2
    ) %>% all
  )
  # ex
  expect_true(
    dplyr::near(
      single_life_table$ex,
      schleswig_ma$ex,
      tol = 0.1
    ) %>% all
  )
  # rel_popx
  # missing
})
