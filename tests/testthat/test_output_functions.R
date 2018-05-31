context("as.mortaar_life_table_list")

test_that("as transformation works or fails depending on input dataset", {
  expect_s3_class(
    as.mortaar_life_table_list(list(a_live_table[[1]], a_live_table[[2]])),
    "mortaar_life_table_list"
  )
  expect_error(
    as.mortaar_life_table_list(list(a_live_table[[1]], 3)),
    NULL
  )
  expect_error(
    as.mortaar_life_table_list(3),
    NULL
  )
})

context("as.mortaar_life_table")

test_that("as works or fails depending on input dataset", {
  expect_s3_class(
    as.mortaar_life_table(data.frame(a = c(20, 20, 20), Dx = c(10, 15, 20))),
    "mortaar_life_table"
  )
  expect_error(
    as.mortaar_life_table(data.frame(a = c(20, 20, 20), Dy = c(10, 15, 20))),
    NULL
  )
  expect_error(
    as.mortaar_life_table(3),
    NULL
  )
})

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
  expect_match(format(a_live_table), "mortAAR")
})

test_that("format.mortaar_life_table_list returns a character string", {
  expect_is(format(a_live_table),"character")
})

test_that("format.mortaar_life_table_list returns no output", {
  expect_silent(format(a_live_table))
})

context("format.mortaar_life_table")

test_that("format is delegated to format.mortaar_life_table on a mortaar_life_table", {
  expect_match(format(a_live_table$schleswig_ma), "mortAAR")
})

test_that("format.mortaar_life_table returns a character string", {
  expect_is(format(a_live_table$schleswig_ma),"character")
})

test_that("format.mortaar_life_table returns no output", {
  expect_silent(format(a_live_table$schleswig_ma))
})

context("print.mortaar_life_table")

test_that("print is delegated to print.mortaar_life_table on a mortaar_life_table", {
  expect_output(print(a_live_table$schleswig_ma), "mortAAR")
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
  expect_output(print(a_live_table), "mortAAR")
})

# test_that("print outputs correctly", {
#   this_life_table <- life.table(data.frame(feschleswig_ma = c(1), indet = c(1)))
#   expect_output(print(this_life_table), "Mortaar life table for feschleswig_ma of 1 individuals")
#   expect_output(print(this_life_table), "Mortaar life table for indet of 1 individuals")
# })

## Plots

# send plot result to /dev/null
pdf(file = NULL)

# Dummy test for plots

context("plot.mortaar_life_table_list")

test_that("plot produces no error", {
  expect_error(plot( a_live_table, display = "dx" ), NA)
  expect_error(plot( a_live_table, display = "qx" ), NA)
  expect_error(plot( a_live_table, display = "lx" ), NA)
  expect_error(plot( a_live_table, display = "ex" ), NA)
  expect_error(plot( a_live_table, display = "rel_popx" ), NA)
})

test_that("plot with system graphics produces no error", {
  expect_error(plot( a_live_table, display = "dx", prefer.ggplot=FALSE ), NA)
  expect_error(plot( a_live_table, display = "qx", prefer.ggplot=FALSE ), NA)
  expect_error(plot( a_live_table, display = "lx", prefer.ggplot=FALSE ), NA)
  expect_error(plot( a_live_table, display = "ex", prefer.ggplot=FALSE ), NA)
  expect_error(plot( a_live_table, display = "rel_popx", prefer.ggplot=FALSE ), NA)
})

test_that("plot life table list produces no error", {
  expect_error(plot( a_live_table) , NA)
})

context("plot.mortaar_life_table")

test_that("plot produces no error", {
  expect_error(plot( a_live_table$schleswig_ma, display = "dx" ), NA)
  expect_error(plot( a_live_table$schleswig_ma, display = "qx" ), NA)
  expect_error(plot( a_live_table$schleswig_ma, display = "lx" ), NA)
  expect_error(plot( a_live_table$schleswig_ma, display = "ex" ), NA)
  expect_error(plot( a_live_table$schleswig_ma, display = "rel_popx" ), NA)
})

test_that("plot with system graphics produces no error", {
  expect_error(plot( a_live_table$schleswig_ma, display = "dx", prefer.ggplot=FALSE ), NA)
  expect_error(plot( a_live_table$schleswig_ma, display = "qx", prefer.ggplot=FALSE ), NA)
  expect_error(plot( a_live_table$schleswig_ma, display = "lx", prefer.ggplot=FALSE ), NA)
  expect_error(plot( a_live_table$schleswig_ma, display = "ex", prefer.ggplot=FALSE ), NA)
  expect_error(plot( a_live_table$schleswig_ma, display = "rel_popx", prefer.ggplot=FALSE ), NA)
})

test_that("plot individual life table dx system graphics produces no error", {
  x <- a_live_table$schleswig_ma
  n <- sum(x$Dx)
  my_subsets = "data set"

  expect_error(mortAAR:::mortaar_plot_dx_frame(x, my_subsets, n=n), NA)
  expect_error(mortAAR:::mortaar_plot_dx(x), NA)
})

test_that("plot individual life table qx system graphics produces no error", {
  x <- a_live_table$schleswig_ma
  n <- sum(x$Dx)
  my_subsets = "data set"

  expect_error(mortAAR:::mortaar_plot_qx_frame(x, my_subsets, n=n), NA)
  expect_error(mortAAR:::mortaar_plot_qx(x), NA)
})

test_that("plot individual life table lx system graphics produces no error", {
  x <- a_live_table$schleswig_ma
  n <- sum(x$Dx)
  my_subsets = "data set"

  expect_error(mortAAR:::mortaar_plot_lx_frame(x, my_subsets, n=n), NA)
  expect_error(mortAAR:::mortaar_plot_lx(x), NA)
})

test_that("plot individual life table ex system graphics produces no error", {
  x <- a_live_table$schleswig_ma
  n <- sum(x$Dx)
  my_subsets = "data set"

  expect_error(mortAAR:::mortaar_plot_ex_frame(x, my_subsets, n=n), NA)
  expect_error(mortAAR:::mortaar_plot_ex(x), NA)
})

test_that("plot individual life table rel_popx system graphics produces no error", {
  x <- a_live_table$schleswig_ma
  n <- sum(x$Dx)
  my_subsets = "data set"

  expect_error(mortAAR:::mortaar_plot_rel_popx_frame(x, my_subsets, n=n), NA)
  expect_error(mortAAR:::mortaar_plot_rel_popx(x), NA)
})

test_that("plot individual life table produces no error", {
    expect_error(plot( a_live_table$schleswig_ma) , NA)
})

dev.off()
