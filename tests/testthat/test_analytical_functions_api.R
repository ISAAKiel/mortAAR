context("life.table error fetching")

test_that("a mortaar_life_table_list is only produced from a list", {
  expect_error(life.table(an_input_dataset), NA)
  expect_error(life.table(c(1,2)), "The input data is not a list.")
})

test_that("a mortaar_life_table is produced from a data.frame, results in a dataframe", {
  expect_error(life.table(an_input_dataset$schleswig_ma), NA)
  expect_is(life.table(an_input_dataset$schleswig_ma),"mortaar_life_table")
  expect_is(life.table(an_input_dataset$schleswig_ma),"data.frame")
  expect_false("list" %in% class(life.table(an_input_dataset$schleswig_ma)))
})

test_that("a mortaar_life_table is produced from a list results in a list", {
  expect_is(life.table(an_input_dataset),"list")
  expect_is(life.table(an_input_dataset),"mortaar_life_table_list")
  expect_false("data.frame" %in% class(life.table(an_input_dataset)))
})

test_that("a mortaar_life_table_list is only produced from a list of data.frames.", {
  a_wrong_input_dataset <- list(data.frame(a = 1:10), "Flööt!")
  expect_error(life.table(a_wrong_input_dataset),"The input list contains at least one element that is not a data.frame. The elements with the following IDs are not data.frames: 2")
  expect_error(life.table(an_input_dataset), NA)
})

test_that("a mortaar_life_table_list is only produced from a list of data.frames
          with the numeric columns 'a' and 'Dx'.", {
            a_wrong_input_dataset <- list(data.frame(a = 1:10, Dx = 1:10), data.frame(a = 1:10))
            expect_error(life.table(a_wrong_input_dataset),"The data.frames with the following element IDs in the input list do not have the numeric columns 'a' and 'Dx': 2")
            expect_error(life.table(an_input_dataset), NA)
          })

test_that("life.table warns if more then the necessary columns are in the input data", {
  a_wrong_input_dataset <- data.frame(a = 1:10, Dx = 1:10, c= 1:10)
  expect_warning(life.table(a_wrong_input_dataset), "Note that these additional columns will be dropped in the output.")
})

test_that("given column x life.table produces column x_auto", {
  expect_true("x_auto" %in% colnames(life.table(data.frame(x=1:10, a = 1:10, Dx = 1:10))))
})

test_that("life.table warns if more age correction values than age classes are given", {
  expect_message(
    suppressWarnings(life.table(data.frame(x=1:10, a = 1:10, Dx = 1:10), agecorfac = 1:11)),
                 "There can not be more age correction factors")
})

