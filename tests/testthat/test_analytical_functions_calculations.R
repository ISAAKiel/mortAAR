context("life.table calculations")

test_that("life.table does the mathing correctly", {
  # dx
  expect_equal(
    single_life_table$Dx / sum(single_life_table$Dx) * 100,
    single_life_table$dx
  )
  # lx
  expect_equal(
    c(100, 100 - cumsum(single_life_table$dx))[1:nrow(single_life_table)],
    single_life_table$lx
  )
  # qx
  expect_equal(
    single_life_table$dx/single_life_table$lx * 100,
    single_life_table$qx
  )
  # Lx
  expect_equal(
    single_life_table$a *
      (single_life_table$lx +
        c(single_life_table$lx[2:nrow(single_life_table)], 0)
      ) / 2,
    single_life_table$Lx
  )
  # Tx
  expect_equal(
    rev(cumsum(rev(single_life_table$Lx))),
    single_life_table$Tx
  )
  # ex
  expect_equal(
    single_life_table$Tx / single_life_table$lx,
    single_life_table$ex
  )
  # Jx
  # missing
  # Ax
  # missing
})
