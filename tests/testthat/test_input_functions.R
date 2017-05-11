context("summe")

test_that("summe replaces NA by 0 and adds up the vectors elementwise", {
  expect_equal(summe(c(1,2,NA,4),c(5,6,7,8)), c(6, 8, 7, 12))
})

context("prep.life.table")

test_that("prep.life.table imports a data set correct", {
  a_example_raw_dataset <- read.csv("Siedlungsbestattungen_ueberblick_for_prep_function.csv")
  load("siedlungsbestattungen_importiert.rda")
  expect_equal(prep.life.table(a_example_raw_dataset,dec="Anzahl.von.Individuum_nr",agebeg = "from", ageend = "to", grnam = "Geschlecht_kombiniert"), prep_output)
})
