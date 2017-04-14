context("is.mortaar_life_table_list")

test_that("a mortaar_life_table_list identifies correctly", {
  expect_true(is.mortaar_life_table_list(a_live_table))
  expect_false(is.mortaar_life_table_list(an_input_dataset))
  })

context("is.mortaar_life_table")

test_that("a mortaar_life_table_list identifies correctly", {
  expect_true(is.mortaar_life_table(a_live_table$schleswig_ma))
  expect_false(is.mortaar_life_table(a_live_table))
  expect_false(is.mortaar_life_table(an_input_dataset))
})

context("format.mortaar_life_table_list")

test_that("format is delegated to format.mortaar_life_table_list on a mortaar_life_table_list", {
  expect_match(format(a_live_table), "Mortaar")
})

test_that("format.mortaar_life_table_list returns a character string", {
  expect_is(format(a_live_table),"character")
})

test_that("format.mortaar_life_table_list returns no output", {
  expect_silent(format(a_live_table))
})

context("format.mortaar_life_table")

test_that("format is delegated to format.mortaar_life_table on a mortaar_life_table", {
  expect_match(format(a_live_table$schleswig_ma), "Mortaar")
})

test_that("format.mortaar_life_table returns a character string", {
  expect_is(format(a_live_table$schleswig_ma),"character")
})

test_that("format.mortaar_life_table returns no output", {
  expect_silent(format(a_live_table$schleswig_ma))
})

context("print.mortaar_life_table")

test_that("print is delegated to print.mortaar_life_table on a mortaar_life_table", {
  expect_output(print(a_live_table$schleswig_ma), "Mortaar")
})

# test_that("print outputs correctly", {
#   this_life_table <- life.table(data.frame(indet = c(1)))$indet
#   expect_output(print(this_life_table), "Mortaar life table of 1 individuals")
#   expect_output(print(this_life_table), "Life expectation at birth")
#   expect_output(print(this_life_table), "age.class")
#   expect_output(print(this_life_table), "0-1")
# })

context("print.mortaar_life_table_list")

test_that("print is delegated to print.mortaar_life_table_list on a mortaar_life_table_list", {
  expect_output(print(a_live_table), "Mortaar")
})

# test_that("print outputs correctly", {
#   this_life_table <- life.table(data.frame(feschleswig_ma = c(1), indet = c(1)))
#   expect_output(print(this_life_table), "Mortaar life table for feschleswig_ma of 1 individuals")
#   expect_output(print(this_life_table), "Mortaar life table for indet of 1 individuals")
# })
