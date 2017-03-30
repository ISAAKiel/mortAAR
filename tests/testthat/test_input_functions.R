context("summe")

test_that("summe replaces NA by 0 and adds up the vectors elementwise", {
  expect_equal(summe(c(1,2,NA,4),c(5,6,7,8)), c(6, 8, 7, 12))
})
