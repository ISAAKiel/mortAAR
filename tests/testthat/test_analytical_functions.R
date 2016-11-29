context("life.table error fetching")

test_that("a mortaar_life_table_list is only produced from a data.frame", {
  expect_error(life.table(an_input_dataset), NA)
  expect_error(life.table(c(1,2)), "The input data is not a data.frame")
})
