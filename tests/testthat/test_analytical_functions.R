context("life.table error fetching")

test_that("a mortaar_life_table_list is only produced from a data.frame", {
  expect_error(life.table(an_input_dataset), NA)
  expect_error(life.table(c(1,2)), "The input data is not a data.frame")
})

test_that("life.table fetches non numeric colums", {
  a_wrong_input_dataset <- data.frame(male = rep(c("alt", "jung"),10))
  expect_error(life.table(a_wrong_input_dataset),"The input data.frame has non-numeric columns. The following column is not numeric: male")
  expect_error(life.table(an_input_dataset),NA)
})
